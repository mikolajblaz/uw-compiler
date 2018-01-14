module Llvm.Generator where

import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map

import AbsLatte

import Llvm.Core
import Llvm.State
import qualified Llvm.Emitter as Emitter
import qualified Llvm.StdLib as Lib


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
    Nothing -> return []
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
  _ <- genRhs expr
  return ()

genStmt (Ass _ ident expr) = do
  rhsAddr <- genRhs expr
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
  addr <- genRhs expr
  genRet addr

genStmt (VRet _) = genVRet

-- declarations
-- process single declaration
genStmt (Decl pos ty [NoInit pos2 ident]) = do
  genStmt (Decl pos ty [Init pos2 ident (defaultInit ty)])

genStmt (Decl _ ty [Init _ ident expr]) = do
  rhsAddr <- genRhs expr
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

-- TODO genRhs -> genRhs

genRhs :: Expr Pos -> GenM Addr
genRhs (EVar pos ident) = do
  (ty, _, addr) <- getIdentVal pos ident
  r <- freshRegister (plainType ty)
  Emitter.emitLoad addr r
  return r

genRhs (ELitInt _ int) = return $ AImm int TInt
genRhs (ELitTrue _) = return $ AImm 1 TBool
genRhs (ELitFalse _) = return $ AImm 0 TBool

genRhs (EApp pos ident exprs) = do
  addrs <- mapM genRhs exprs
  (_, _, funAddr) <- getIdentVal pos ident
  let retTy = getAddrType funAddr
  case retTy of
    TVoid -> do
               Emitter.emitVoidCall funAddr addrs
               return undefined -- it will not be used anyway
    _ -> do
          r <- freshRegister retTy
          Emitter.emitCall r funAddr addrs
          return r

genRhs (EString _ str) = do
  -- addr <- insertStringConstant str -- TODO
  return $ AStr (UIdent "" 0) TStr -- TODO

genRhs (Neg p expr) = genBinOp "sub" (ELitInt p 0) expr
-- logical not is a substraction from True, when operating on 'i1' type
genRhs (Not p expr) = genBinOp "sub" (ELitTrue p) expr

genRhs (EMul _ expr op expr2) = genBinOp opName expr expr2
  where
    opName = case op of
      Times _ -> "mul"
      Div _ -> "sdiv"
      Mod _ -> "srem"

-- HEAD
genRhs (EAdd _ expr (Minus _) expr2) = genBinOp "sub" expr expr2

genRhs (EAdd pos expr (Plus _) expr2) = do
  addr <- genRhs expr
  addr2 <- genRhs expr2
  let ty = getAddrType addr
  r <- freshRegister ty;
  case getAddrType addr of
    TInt -> Emitter.emitBinOp "add" r addr addr2
    TStr -> do
              -- TODO check
              (_, _, funAddr) <- getIdentVal Nothing (Ident "concat")
              Emitter.emitCall r funAddr [addr, addr2]
    _ -> failPos pos $ "Compiler error" -- this shouldn't happen after type check
  return r

genRhs (ERel pos expr rel expr2) = do
  addr <- genRhs expr
  addr2 <- genRhs expr2
  r <- freshRegister TBool
  let ty = getAddrType addr -- = getAddrType addr2 (thanks to type check)
  case ty of
    TInt -> Emitter.emitCmp r rel 's' addr addr2
    TBool -> Emitter.emitCmp r rel 'u' addr addr2
    TStr -> do
              (_, _, funAddr) <- getIdentVal Nothing (Ident "compareStrings")
              Emitter.emitCall r funAddr [addr, addr2]
    _ -> failPos pos $ "Compiler error" -- this shouldn't happen after type check
  return r

-- remaining exps are EAnd and EOr, both handled the same way:
genRhs e = do
  trueLabel <- freshLabel
  falseLabel <- freshLabel
  afterLabel <- freshLabel
  genCond e trueLabel falseLabel

  setCurrentBlock trueLabel
  Emitter.emitComment $ "in block "++ show afterLabel ++ ", set a proper value using a phi construct"
  genJmp afterLabel
  setCurrentBlock falseLabel
  genJmp afterLabel

  setCurrentBlock afterLabel
  r <- freshRegister TBool
  Emitter.emitPhi r [(AImm 1 TBool, ALab trueLabel), (AImm 0 TBool, ALab falseLabel)]
  return r


genBinOp :: String -> Expr Pos -> Expr Pos -> GenM Addr
genBinOp opName expr expr2 = do
  addr <- genRhs expr
  addr2 <- genRhs expr2
  r <- freshRegister (getAddrType addr)
  Emitter.emitBinOp opName r addr addr2
  return r


genLhs :: Ident -> GenM Addr
genLhs ident = do
  (_, _, addr) <- getIdentVal Nothing ident
  return addr


------------------------- Conditions ----------------------------------------
-- | Lazy condition checking by a "jumping code"
genCond :: Expr Pos -> Label -> Label -> GenM ()
genCond (EAnd _ expr expr2) trueLabel falseLabel = do
  midLabel <- freshLabel
  genCond expr midLabel falseLabel

  setCurrentBlock midLabel
  genCond expr2 trueLabel falseLabel

genCond (EOr _ expr expr2) trueLabel falseLabel = do
  midLabel <- freshLabel
  genCond expr trueLabel midLabel

  setCurrentBlock midLabel
  genCond expr2 trueLabel falseLabel

genCond expr trueLabel falseLabel = do
  addr <- genRhs expr
  genBr addr trueLabel falseLabel

------------------------- Terminators ---------------------------------------
-- NOTE: all those (and only those) finish a block
genJmp :: Label -> GenM ()
genJmp label = do
  Emitter.emitJmp (ALab label)
  finishBlock

genBr :: Addr -> Label -> Label -> GenM ()
genBr flag trueLabel falseLabel = do
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
