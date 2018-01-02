module Llvm.Generator where

import Control.Monad ( liftM, when )

import AbsLatte
import LexLatte
import ParLatte
import ErrM

import Llvm.Core
import Llvm.State
-- import Llvm.Emitter


genStmt :: Stmt Pos -> GenM ()
genStmt = undefined


genExpr :: Expr Pos -> GenM Addr
genExpr = undefined

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
