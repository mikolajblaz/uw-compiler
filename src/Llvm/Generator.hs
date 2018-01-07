module Llvm.Generator (genStmt) where

import Control.Monad ( liftM, when )

import AbsLatte
import LexLatte
import ParLatte
import ErrM

import Llvm.Core
import Llvm.State
import qualified Llvm.Emitter as Emitter


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
genStmt :: Maybe Label -> Stmt Pos -> GenM ()
genStmt nextL (Empty _) = do
  maybeJump nextL

genStmt nextL (SExp _ expr) = do
  genExpr expr
  maybeJump nextL

genStmt nextL (Ass _ ident expr) = do
  rhsAddr <- genExpr expr
  lhsAddr <- genLhs ident
  ty <- getIdentType ident
  genAss ty lhsAddr rhsAddr
  maybeJump nextL

genStmt nextL (Decl _ ty [Init _ ident expr]) = do
  rhsAddr <- genExpr expr
  -- NOTE: here environment changes
  insertLocalDecl ident ty
  genAlloc ident ty
  lhsAddr <- genLhs ident
  genAss ty lhsAddr rhsAddr
  maybeJump nextL

genStmt nextL (Incr pos ident) =
  genStmt nextL (Ass pos ident identPlus1)
    where
      identPlus1 = EAdd pos (EVar pos ident) (Plus pos) (ELitInt pos 1)

genStmt nextL (Decr pos ident) =
  genStmt nextL (Ass pos ident identMinus1)
    where
      identMinus1 = EAdd pos (EVar pos ident) (Minus pos) (ELitInt pos 1)

genStmt nextL (Cond _ cond thenStmt) = do
  afterLabel <- establishNextLabel nextL
  thenLabel <- freshLabel
  genCond cond thenLabel afterLabel

  setCurrentBlock thenLabel
  genStmt (Just afterLabel) thenStmt

  setCurrentBlock afterLabel

genStmt nextL (CondElse _ cond thenStmt elseStmt) = do
  afterLabel <- establishNextLabel nextL
  thenLabel <- freshLabel
  elseLabel <- freshLabel
  genCond cond thenLabel elseLabel

  setCurrentBlock thenLabel
  genStmt (Just afterLabel) thenStmt

  setCurrentBlock elseLabel
  genStmt (Just afterLabel) elseStmt

  setCurrentBlock afterLabel

genStmt nextL (While _ cond bodyStmt) = do
  afterLabel <- establishNextLabel nextL
  bodyLabel <- freshLabel
  condLabel <- freshLabel

  genJump condLabel

  -- In Llvm it doesn't matter, but the order is as in an efficient assembler
  -- First body, then condition
  setCurrentBlock bodyLabel
  genStmt (Just condLabel) bodyStmt

  setCurrentBlock condLabel
  genCond cond bodyLabel afterLabel

  setCurrentBlock afterLabel


genStmt _ (Ret _ expr) = do
  addr <- genExpr expr
  genRet addr

genStmt _ (VRet _) = genVRet


-- these shouldn't happen
genStmt _ (BStmt pos _) = failPos pos $ "Compiler error"
-- single decl already handled
genStmt _ (Decl pos _ _) = failPos pos $ "Compiler error"



-- Assignment
genAss :: Type Pos -> Addr -> Addr -> GenM ()
genAss ty lhsAddr rhsAddr = do -- TODO emitAss?
  undefined
  -- TODO

genAlloc :: Ident -> Type Pos -> GenM ()
genAlloc = undefined


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
  genCondJump addr trueLabel falseLabel


genCmp :: RelOp Pos -> Addr -> Addr -> GenM Addr
genCmp rel lAddr rAddr = do
  resAddr <- freshRegister TBool
  Emitter.emitCmp resAddr rel lAddr rAddr
  return resAddr

------------------------- Jumps ---------------------------------------------

maybeJump :: Maybe Label -> GenM ()
maybeJump Nothing = return ()
maybeJump (Just nextL) = genJump nextL

-- Important:
-- All those (and only those) finish a block
-- TODO all these must call finishBlock!
genJump :: Label -> GenM ()
genJump = undefined

genCondJump :: Addr -> Label -> Label -> GenM ()
genCondJump flag trueLabel falseLabel = undefined

genRet :: Addr -> GenM ()
genRet = undefined

genVRet :: GenM ()
genVRet = undefined

{-|
------------ Instructions generation ---------------------
-- Return all generated instructions
genProgram :: Program -> GenM ()
genProgram (Prog stmts) = do
  emitDeclarations
  genMainFunction stmts

genMainFunction :: [Stmt] -> GenM ()
genMainFunction stmts = do
  emitFunctionHeader
  genStmts stmts
  emitFunctionEnd

----------- Statement compilation --------------------
genStmts :: [Stmt] -> GenM ()
genStmts stmts = do
  mapM_ genStmt stmts

genStmt :: Stmt -> GenM ()
genStmt (SAss ident exp) = do
  r <- genExp exp
  (a, newlyAllocated) <- getAddrOrAlloc ident
  when newlyAllocated $ emitAlloc a
  emitStore r a

genStmt (SExp exp) = do
  r <- genExp exp
  emitPrintInt r

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
