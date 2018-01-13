module Llvm.Frontend (analyzeProgram) where

import Control.Monad ( foldM, liftM, unless )
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import Data.Maybe

import Llvm.Core
import Llvm.State
import qualified Llvm.LibraryDecl as Lib

import AbsLatte
import ErrM

----------------------- Main function  ---------------------------------------
-- check if main exists and check type
checkMain :: GenM ()
checkMain = do
  (ty, _, _) <- getIdentVal Nothing (Ident "main")
  unless ((getTypePos ty) == (TFun TInt [])) $ failPos pos $
    "Type error: the 'main' function must have a signature 'int main ()"
  return ()


----------------------- Type check -------------------------------------------
forbidVoid :: Type Pos -> GenM ()
forbidVoid (Void pos) = failPos pos $ "Type error, variables cannot have void type"
forbidVoid _ = return ()

checkEqual :: Pos -> Type Pos -> Type Pos -> GenM ()
checkEqual pos ty expTy =
  unless ((plainType ty) == (plainType expTy)) $
    gets outerEnv >>= (\oe ->
    failPos pos $
    "Type error, got: " ++ printLatte expTy ++ ", expected: " ++ printLatte ty ++ "\n\n" ++ show oe)


------------------------ Analysis --------------------------------------------
analyzeProgram :: Program Pos -> GenM (Program Pos)
analyzeProgram (Program pos topDefs) = do
  buildTopEnv (Lib.libraryTopDefs ++ topDefs)
  checkMain
  -- NOTE: we don't analyze libraryTopDefs, we just put it to topEnv
  newTopDefs <- mapM analyzeTopDef topDefs
  return $ Program pos newTopDefs


analyzeTopDef :: TopDef Pos -> GenM (TopDef Pos)
analyzeTopDef (FnDef pos ty ident@(Ident i) args block) = do
  startNewFun ident ty
  analyzeArgs args
  -- TODO set outer env

  (newBlock, endsRet) <- analyzeBlock block

  unless (endsRet || (plainType ty == TVoid)) $
    fail $ "Error: no return instruction in function " ++ show i

  return $ FnDef pos ty ident args newBlock

-- | Update environment
-- | Check voids
-- | Init variables
-- | Copy values from arguments
-- | I.e. store i32, i32 %n, i32* %loc_n
analyzeArgs :: [Arg Pos] -> GenM ()
analyzeArgs args = return ()
  -- TODO


-- Return possibly change abstract tree and flag inidicating whether
-- block ends with a return
analyzeBlock :: Block Pos -> GenM (Block Pos, Bool)
analyzeBlock (Block pos []) = return (Block pos [], False)
analyzeBlock (Block pos stmts) = do
  oldBlockEnv <- gets blockEnv
  oldOuterEnv <- gets outerEnv
  let newOuterEnv = blockToOuterEnv oldBlockEnv oldOuterEnv
  setNewEnvs emptyEnv newOuterEnv
  -- Now, analyze block in an empty block environment
  (newStmts, endsRets) <- liftM unzip $ mapM analyzeStmt stmts
  -- if any instruction ecnd with ret, the block ends with ret also
  let endsRet = or endsRets
  -- TODO trim instructions after ret
  -- Set old environment back, discarding everything that was inside the block
  setNewEnvs oldBlockEnv oldOuterEnv
  return $ (Block pos newStmts, endsRet)


  -- Return possibly change abstract tree and flag inidicating whether
  -- statment ends with a return
analyzeStmt :: Stmt Pos -> GenM (Stmt Pos, Bool)
analyzeStmt (BStmt pos b) = do
  (newB, endsRet) <- analyzeBlock b
  return $ (BStmt pos newB, endsRet)

-- analyze single declaration
analyzeStmt d@(Decl pos ty [NoInit pos2 ident]) = do
  forbidVoid ty
  insertLocalDecl ident ty
  return (d, False)

analyzeStmt (Decl pos ty [Init pos2 ident expr]) = do
  forbidVoid ty
  (newExpr, exprTy) <- analyzeExpr expr
  checkEqual pos ty exprTy
  insertLocalDecl ident ty
  return (Decl pos ty [Init pos2 ident newExpr], False)

-- multiple declarations
analyzeStmt (Decl pos ty items) = do
  let singleDecls = map (\item -> Decl pos ty [item]) items
  declsList <- mapM analyzeStmt singleDecls
  return $ (Decl pos ty $ map extractInit declsList, False)
    where
      extractInit (Decl _ _ [init], _) = init


analyzeStmt (Empty pos) = return (Empty pos, False)

analyzeStmt (SExp pos expr) = do
  (newExpr, exprTy) <- analyzeExpr expr
  -- TODO type?
  return (SExp pos newExpr, False)

analyzeStmt (Ass pos ident expr) = do
  (newExpr, exprTy) <- analyzeExpr expr
  ty <- getIdentType pos ident
  checkEqual pos ty exprTy
  return (Ass pos ident newExpr, False)

analyzeStmt (Incr pos ident) = do
  ty <- getIdentType pos ident
  checkEqual pos (Int Nothing) ty
  let identPlus1 = EAdd pos (EVar pos ident) (Plus pos) (ELitInt pos 1)
  return (Ass pos ident identPlus1, False)

analyzeStmt (Decr pos ident) = do
  ty <- getIdentType pos ident
  checkEqual pos (Int Nothing) ty
  let identMinus1 = EAdd pos (EVar pos ident) (Minus pos) (ELitInt pos 1)
  return (Ass pos ident identMinus1, False)

analyzeStmt (Cond pos cond thenStmt) = do
  newCond <- analyzeCond pos cond
  (newThen, endsRetThen) <- analyzeStmt thenStmt
  -- TODO check if there was a return already
  return $ case newCond of
    ELitTrue _ -> (newThen, endsRetThen)
    ELitFalse pos -> (Empty pos, False)
    _ -> (Cond pos newCond newThen, False)

analyzeStmt (CondElse pos cond thenStmt elseStmt) = do
  newCond <- analyzeCond pos cond
  (newThen, endsRetThen) <- analyzeStmt thenStmt
  (newElse, endsRetElse) <- analyzeStmt elseStmt
  -- TODO check if there was a return already
  return $ case newCond of
    ELitTrue _ -> (newThen, endsRetThen)
    ELitFalse _ -> (newElse, endsRetElse)
    _ -> (CondElse pos newCond newThen newElse, endsRetThen && endsRetElse)

analyzeStmt (While pos cond bodyStmt) = do
  newCond <- analyzeCond pos cond
  (newBody, endsRetBody) <- analyzeStmt bodyStmt
  return $ case newCond of
    ELitTrue _ -> (While pos newCond newBody, endsRetBody)
    ELitFalse _ -> (Empty pos, False)
    _ -> (While pos newCond newBody, False)

analyzeStmt (Ret pos expr) = do
  (newExpr, exprTy) <- analyzeExpr expr
  retTy <- gets currentFunRetType
  checkEqual pos retTy exprTy
  return (Ret pos newExpr, True)

analyzeStmt (VRet pos) = do
  retTy <- gets currentFunRetType
  checkEqual pos retTy (Void Nothing)
  return (VRet pos, True)


analyzeCond :: Pos -> Expr Pos -> GenM (Expr Pos)
analyzeCond pos cond = do
  (newExpr, exprTy) <- analyzeExpr cond
  checkEqual pos exprTy (Bool Nothing)
  return newExpr


analyzeExpr :: Expr Pos -> GenM (Expr Pos, Type Pos)
analyzeExpr e@(EVar pos ident) = do
  ty <- getIdentType pos ident
  return (e, ty)

analyzeExpr e@(ELitInt pos _) = do
  return (e, Int pos)

analyzeExpr e@(ELitTrue pos) = do
  return (e, Bool pos)

analyzeExpr e@(ELitFalse pos) = do
  return (e, Bool pos)

analyzeExpr (EApp pos ident@(Ident i) exprs) = do
  funTy <- getIdentType pos ident
  (retTy, argsTys) <- case funTy of
    (Fun _ retTy argsTys) -> return (retTy, argsTys)
    _ -> failPos pos $ show i ++ "is not a function to apply"

  (newExprs, exprTys) <- liftM unzip $ mapM analyzeExpr exprs
  unless (length argsTys == length exprTys) $ failPos pos $
    "Wrong number of arguments to apply to function " ++ show i
  -- check arguments type equality
  mapM_ (uncurry (checkEqual pos)) $ zip exprTys argsTys
  return (EApp pos ident newExprs, retTy)

analyzeExpr e@(EString pos _) = do
  return (e, Str pos)

analyzeExpr (Neg pos expr) = do
  (newExpr, exprTy) <- analyzeExpr expr
  checkEqual pos exprTy (Int Nothing)
  return (Neg pos newExpr, exprTy)

analyzeExpr (Not pos expr) = do
  (newExpr, exprTy) <- analyzeExpr expr
  checkEqual pos exprTy (Bool Nothing)
  return (Not pos newExpr, exprTy)

analyzeExpr (EMul pos expr op expr2) = do
  (newExpr, exprTy) <- analyzeExpr expr
  (newExpr2, exprTy2) <- analyzeExpr expr2
  checkEqual pos exprTy (Int Nothing)
  checkEqual pos exprTy2 (Int Nothing)
  return (EMul pos newExpr op newExpr2, Int pos)

analyzeExpr (EAdd pos expr op expr2) = do
  (newExpr, exprTy) <- analyzeExpr expr
  (newExpr2, exprTy2) <- analyzeExpr expr2
  case op of
    Minus _ -> checkEqual pos exprTy (Int Nothing)
    Plus _ -> case exprTy of
      Int _ -> return ()
      Str _ -> return ()
      _ -> failPos pos $ "Expected int or string, got: " ++ printLatte exprTy

  checkEqual pos exprTy exprTy2
  return (EAdd pos newExpr op newExpr2, exprTy)

analyzeExpr (ERel pos expr op expr2) = do
  (newExpr, exprTy) <- analyzeExpr expr
  (newExpr2, exprTy2) <- analyzeExpr expr2
  checkEqual pos exprTy (Int Nothing)
  checkEqual pos exprTy2 (Int Nothing)
  return (ERel pos newExpr op newExpr2, Bool pos)

analyzeExpr (EAnd pos expr expr2) = do
  (newExpr, exprTy) <- analyzeExpr expr
  (newExpr2, exprTy2) <- analyzeExpr expr2
  checkEqual pos exprTy (Bool Nothing)
  checkEqual pos exprTy2 (Bool Nothing)
  return (EAnd pos newExpr newExpr2, Bool pos)

analyzeExpr (EOr pos expr expr2) = do
  (newExpr, exprTy) <- analyzeExpr expr
  (newExpr2, exprTy2) <- analyzeExpr expr2
  checkEqual pos exprTy (Bool Nothing)
  checkEqual pos exprTy2 (Bool Nothing)
  return (EOr pos newExpr newExpr2, Bool pos)
