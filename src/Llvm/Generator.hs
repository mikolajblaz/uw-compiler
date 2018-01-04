module Llvm.Generator (genStmt) where

import Control.Monad ( liftM, when )

import AbsLatte
import LexLatte
import ParLatte
import ErrM

import Llvm.Core
import Llvm.State
-- import qualified Llvm.Emitter as Emitter


-- Invariant #1
-- when genStmt is called, there is some block started already
-- (stored in state in currentBlock)

-- Invariant #2
-- a function calling
--      l <- getNewBlockLabel
-- will eventually call
--      setCurrentBlock l

-- TODO but who starts block e.g. in Empty below?
-- TODO possible answer: genStmt Empty doesn't jump, it generates a jump!
-- TODO Inv #2 should be an aswer

genStmt :: Maybe SBlockLabel -> Stmt Pos -> GenM ()
genStmt nextSb (Empty _) = do
  maybeJump nextSb

genStmt nextSb (Cond _ cond thenStmt) = do
  afterLabel <- establishNextLabel nextSb
  thenLabel <- getNewBlockLabel
  genCond cond thenLabel afterLabel

  setCurrentBlock thenLabel
  genStmt afterLabel thenStmt

  setCurrentBlock afterLabel


genStmt nextSb (CondElse _ cond thenStmt elseStmt) = do
  afterLabel <- establishNextLabel nextSb
  thenLabel <- getNewBlockLabel
  elseLabel <- getNewBlockLabel
  genCond cond thenLabel elseLabel

  setCurrentBlock thenLabel
  genStmt afterLabel thenStmt

  setCurrentBlock elseLabel
  genStmt afterLabel elseStmt

  setCurrentBlock afterLabel






{-
| Decl a (Type a) [Item a]
| Ass a Ident (Expr a)
| Incr a Ident
| Decr a Ident
| Ret a (Expr a)
| VRet a
| Cond a (Expr a) (Stmt a)
| CondElse a (Expr a) (Stmt a) (Stmt a)
| While a (Expr a) (Stmt a)
| SExp a (Expr a)
-}


-- these shouldn't happen
genStmt nextSb (BStmt pos _) = failPos pos $ "Compiler error"
-- single decl already handled
genStmt nextSb (Decl pos _ _ _) = failPos pos $ "Compiler error"


maybeJump :: Maybe SBlockLabel -> Gen ()
maybeJump Nothing = return ()
maybeJump (Just nextSb) = genJump nextSb
-- maybeJump = maybe (return ()) genJump -- TODO check this out


establishNextLabel :: Maybe SBlockLabel -> GenM SBlockLabel
establishNextLabel nextSb = maybe getNewBlockLabel id nextSb

------------------------- Jumps ---------------------------------------------

-- Important:
-- All those (and only those) finish a block
-- TODO all these must call finishBlock!
genJump :: SBlockLabel -> GenM ()
genJump = undefined

genCondJump :: Addr -> SBlockLabel -> SBlockLabel -> GenM ()
genCondJump = undefined

genRet :: Expr Pos -> SBlockLabel -> GenM ()
genRet = undefined

genVRet :: SBlockLabel -> GenM ()
genVRet = undefined

------------------------- Expressions ---------------------------------------

genExpr :: Expr Pos -> GenM Addr
genExpr = undefined



------------------------- Conditions ----------------------------------------
genCond :: Expr Pos -> SBlockLabel -> SBlockLabel -> GenM ()

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
