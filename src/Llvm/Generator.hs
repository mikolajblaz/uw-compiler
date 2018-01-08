module Llvm.Generator where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import Data.Maybe

import AbsLatte
import LexLatte
import ParLatte
import ErrM

import Llvm.Core
import Llvm.State
import qualified Llvm.Emitter as Emitter


processProgram :: Program Pos -> GenM String
processProgram (Program _ topDefs) = do
  buildTopEnv topDefs
  let decls = Emitter.outputDeclarations
  funOuts <- mapM processTopDef topDefs
  return $ unlines $ decls ++ (funOuts >>= id)


-- | Return instructions in proper order
processTopDef :: TopDef Pos -> GenM [Instr]
processTopDef (FnDef pos ty ident args block) = do
  startNewFun ident ty
  funArgs <- processArgs args
  -- TODO set outer env
  let funFrame = Emitter.outputFunctionFrame (plainType ty) ident

  entry <- freshLabel
  setCurrentBlock entry
  processBlock block

  funBody <- finishFunction

  return $ funFrame funArgs funBody

-- | Update environment
-- | Check voids
-- | Init variables
-- | Copy values from arguments
-- | I.e. store i32, i32 %n, i32* %loc_n
processArgs :: [Arg Pos] -> GenM String
processArgs args = return ""
  -- TODO



processBlock :: Block Pos -> GenM ()
processBlock (Block _ []) = return ()
processBlock (Block pos stmts) = do
  oldBlockEnv <- gets blockEnv
  oldOuterEnv <- gets outerEnv
  let newOuterEnv = blockToOuterEnv oldBlockEnv oldOuterEnv
  setNewEnvs emptyEnv newOuterEnv
  -- Now, process block in an empty block environment
  mapM_ genStmt stmts
  -- Set old environment back, discarding everything that was inside the block
  setNewEnvs oldBlockEnv oldOuterEnv



finishFunction :: GenM [Instr] -- TODO
finishFunction = do
  currFun <- gets currentFun
  funBlocks <- gets funBlocks
  simpleBlocks <- gets simpleBlocks
  labelCnt <- gets labelCnt
  let funLabels = [0 .. labelCnt - 1]
  funIstrs <- mapM outputBlock funLabels
  return $ funIstrs >>= id -- flatMap

outputBlock :: Label -> GenM [Instr]
outputBlock label = do
  blocks <- gets simpleBlocks
  let instrs = Map.lookup label blocks
  case instrs of
    Nothing -> return [] -- TODO -- gets simpleBlocks >>= fail . (++ show label) . show
    Just instrs -> return $ (Emitter.printAddr (ALab label) ++ ":") : instrs

------------------------------ Generator part ----------------------------------

establishNextLabel :: Maybe Label -> GenM Label
establishNextLabel nextL = maybe freshLabel return nextL


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
  genExpr expr
  return ()

genStmt (Ass _ ident expr) = do
  rhsAddr <- genExpr expr
  lhsAddr <- genLhs ident
  ty <- getIdentType ident
  Emitter.emitStore lhsAddr rhsAddr

genStmt (Cond _ cond thenStmt) = do
  afterLabel <- freshLabel
  thenLabel <- freshLabel
  genCond cond thenLabel afterLabel

  setCurrentBlock thenLabel
  genStmt thenStmt
  genJmp afterLabel

  -- TODO check if there was a return already?
  setCurrentBlock afterLabel

genStmt (CondElse _ cond thenStmt elseStmt) = do
  afterLabel <- freshLabel
  thenLabel <- freshLabel
  elseLabel <- freshLabel
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
  afterLabel <- freshLabel
  bodyLabel <- freshLabel
  condLabel <- freshLabel

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
  Emitter.emitStore lhsAddr rhsAddr

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
  (_, _, addr) <- getIdentVal ident
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

maybeJmp :: Maybe Label -> GenM ()
maybeJmp Nothing = return ()
maybeJmp (Just nextL) = genJmp nextL

-- NOTE:
-- All those (and only those) finish a block
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

genVRet :: GenM ()
genVRet = do
  Emitter.emitVRet
  finishBlock

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
