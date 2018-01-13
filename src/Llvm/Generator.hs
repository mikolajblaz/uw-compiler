module Llvm.Generator where

import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map

import AbsLatte

import Llvm.Core
import Llvm.State
import qualified Llvm.Emitter as Emitter
import qualified Llvm.LibraryDecl as Lib


processProgram :: Program Pos -> GenM String
processProgram (Program _ topDefs) = do
  buildTopEnv (Lib.libraryTopDefs ++ topDefs)
  let libraryOutput = Lib.printLibraryDeclarations
  -- NOTE: we don't process libraryTopDefs, we just put it to topEnv
  funOuts <- mapM processTopDef topDefs
  return $ unlines $ libraryOutput ++ (concat funOuts)


-- | Return instructions in proper order
processTopDef :: TopDef Pos -> GenM [Instr]
processTopDef (FnDef _ ty ident args block) = do
  startNewFun ident ty
  -- outer env is now set

  -- from now, everything is emitted directly to state
  entry <- freshLabel
  setCurrentBlock entry
  argsAddrs <- processArgs args
  processBlock block

  -- by calling finishFunction, we gather emitted instructions from the state
  funBody <- finishFunction

  let funOut = Emitter.outputFunction (plainType ty) ident argsAddrs funBody
  return funOut

processArgs :: [Arg Pos] -> GenM [Addr]
processArgs [] = return []
processArgs args = do
  Emitter.emitComment "copy argument values:"
  argAddrs <- mapM processArg args
  Emitter.emitEmptyLine  -- TODO remove? from emitter too?
  return argAddrs

-- | Update environment
-- | Copy value from function argument
-- | I.e. store i32, i32 %arg_n, i32* %loc_n
processArg :: Arg Pos -> GenM Addr
processArg (Arg _ ty ident) = do
  locAddr@(ALoc ui ty) <- insertLocalDecl ident ty
  Emitter.emitAlloc locAddr

  let argAddr = AArg ui ty
  Emitter.emitStore argAddr locAddr
  return argAddr


processBlock :: Block Pos -> GenM ()
processBlock (Block _ []) = return ()
processBlock (Block _ stmts) = do
  oldBlockEnv <- gets blockEnv
  oldOuterEnv <- gets outerEnv
  let newOuterEnv = blockToOuterEnv oldBlockEnv oldOuterEnv
  setNewEnvs emptyEnv newOuterEnv
  -- Now, process block in an empty block environment
  mapM_ genStmtCommentWrapper stmts
  -- Set old environment back, discarding everything that was inside the block
  setNewEnvs oldBlockEnv oldOuterEnv



finishFunction :: GenM [Instr] -- TODO
finishFunction = do
  labelCnt <- gets labelCnt
  let funLabels = [0 .. labelCnt - 1]
  funIstrs <- mapM outputBlock funLabels
  return $ concat funIstrs

outputBlock :: Label -> GenM [Instr]
outputBlock label = do
  blocks <- gets simpleBlocks
  let instrs = Map.lookup label blocks
  case instrs of
    Nothing -> return [] -- TODO -- gets simpleBlocks >>= fail . (++ show label) . show
    Just instrs -> return $ (Emitter.printLabelName label ++ ":") : instrs

------------------------------ Generator part ----------------------------------

genStmtCommentWrapper :: Stmt Pos -> GenM ()
genStmtCommentWrapper b@(BStmt _ _) = genStmt b
genStmtCommentWrapper stmt = do
  Emitter.emitCommentStmt stmt
  genStmt stmt

-- genStmt Invariant #1
-- when genStmt is called, there is some block started already
-- (stored in state in currentBlock)

-- genStmt Invariant #2
-- a function calling
--      l <- freshLabel
-- will eventually call
--      setCurrentBlock l
genStmt :: Stmt Pos -> GenM ()
genStmt (BStmt _ b) = processBlock b

genStmt (Empty _) = return ()

genStmt (SExp _ expr) = do
  _ <- genExpr expr
  return ()

genStmt (Ass _ ident expr) = do
  rhsAddr <- genExpr expr
  lhsAddr <- genLhs ident
  Emitter.emitStore rhsAddr lhsAddr

genStmt (Cond _ cond thenStmt) = do
  thenLabel <- freshLabel
  afterLabel <- freshLabel
  genCond cond thenLabel afterLabel

  setCurrentBlock thenLabel
  genStmt thenStmt
  genJmp afterLabel

  -- TODO check if there was a return already?
  setCurrentBlock afterLabel

genStmt (CondElse _ cond thenStmt elseStmt) = do
  thenLabel <- freshLabel
  elseLabel <- freshLabel
  afterLabel <- freshLabel
  genCond cond thenLabel elseLabel

  setCurrentBlock thenLabel
  genStmt thenStmt
  genJmp afterLabel

  setCurrentBlock elseLabel
  genStmt elseStmt
  genJmp afterLabel

  -- TODO check if there was a return already
  setCurrentBlock afterLabel

genStmt (While _ cond bodyStmt) = do
  bodyLabel <- freshLabel
  condLabel <- freshLabel
  afterLabel <- freshLabel

  genJmp condLabel

  -- In Llvm it doesn't matter, but the order is as in an efficient assembler:
  -- First body, then condition
  setCurrentBlock bodyLabel
  genStmt bodyStmt
  genJmp condLabel

  setCurrentBlock condLabel
  genCond cond bodyLabel afterLabel

  -- TODO check if there was a return already
  setCurrentBlock afterLabel


genStmt (Ret _ expr) = do
  addr <- genExpr expr
  genRet addr

genStmt (VRet _) = genVRet

-- declarations
-- process single declaration
genStmt (Decl pos ty [NoInit pos2 ident]) = do
  genStmt (Decl pos ty [Init pos2 ident (defaultInit ty)])

genStmt (Decl _ ty [Init _ ident expr]) = do
  rhsAddr <- genExpr expr
  -- NOTE: here environment changes
  declAddr <- insertLocalDecl ident ty
  Emitter.emitAlloc declAddr
  lhsAddr <- genLhs ident
  Emitter.emitStore rhsAddr lhsAddr

-- multiple declarations
genStmt (Decl pos ty items) = do
  let singleDecls = map (\item -> Decl pos ty [item]) items
  mapM_ genStmt singleDecls


-- these shouldn't happen after context analysis
genStmt (Incr pos _) = failPos pos $ "Compiler error"
genStmt (Decr pos _) = failPos pos $ "Compiler error"

------------------------- Expressions ---------------------------------------

-- TODO genExpr -> genRhs

genExpr :: Expr Pos -> GenM Addr
genExpr _ = return $ AImm 1 TInt -- TODO


genLhs :: Ident -> GenM Addr
genLhs ident = do
  (_, _, addr) <- getIdentVal Nothing ident
  return addr


------------------------- Conditions ----------------------------------------
genCond :: Expr Pos -> Label -> Label -> GenM ()
genCond expr trueLabel falseLabel = do
  addr <- genCmp (LTH Nothing) (AImm 0 TInt) (AImm 1 TInt) -- TODO
  genCondJmp addr trueLabel falseLabel


genCmp :: RelOp Pos -> Addr -> Addr -> GenM Addr
genCmp rel lAddr rAddr = do
  resAddr <- freshRegister TBool
  Emitter.emitCmp resAddr rel lAddr rAddr
  return resAddr

------------------------- Jumps ---------------------------------------------
-- NOTE: all those (and only those) finish a block
genJmp :: Label -> GenM ()
genJmp label = do
  Emitter.emitJmp (ALab label)
  finishBlock

genCondJmp :: Addr -> Label -> Label -> GenM ()
genCondJmp flag trueLabel falseLabel = do
  Emitter.emitBr flag (ALab trueLabel) (ALab falseLabel)
  finishBlock

genRet :: Addr -> GenM ()
genRet a = do
  Emitter.emitRet a
  finishBlock
  afterLabel <- freshLabel
  setCurrentBlock afterLabel

genVRet :: GenM ()
genVRet = do
  Emitter.emitVRet
  finishBlock
  afterLabel <- freshLabel
  setCurrentBlock afterLabel

{-|
----------- Expressions compilation --------------------

-- Return expression address
genExp :: Exp -> GenM Addr
genExp (ExpLit l) = return $ Immediate l
genExp (ExpAdd e1 e2) = genBinOp "add" e1 e2
genExp (ExpSub e1 e2) = genBinOp "sub" e1 e2
genExp (ExpMul e1 e2) = genBinOp "mul" e1 e2
genExp (ExpDiv e1 e2) = genBinOp "sdiv" e1 e2
genExp (ExpVar ident) = do
  a <- getAddr ident
  r <- freshRegister
  emitLoad a r
  return r

genBinOp :: String -> Exp -> Exp -> GenM Addr
genBinOp op e1 e2 = do
  a1 <- genExp e1
  a2 <- genExp e2
  r <- freshRegister
  emitBinOp op r a1 a2
  return r

|-}
