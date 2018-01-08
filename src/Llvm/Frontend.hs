module Llvm.Frontend (analyzeProgram) where

import Control.Monad ( foldM, liftM, unless )
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import Data.Maybe

import Llvm.Core
import Llvm.State

import AbsLatte
import ErrM

----------------------- Main function  ---------------------------------------
-- check if main exists and check type
checkMain :: GenM ()
checkMain = do
  (ty, _, _) <- getIdentVal (Ident "main")
  checkEqual (Fun Nothing (Int Nothing) []) ty
  return () -- TODO


----------------------- Type check -------------------------------------------
forbidVoid :: Type Pos -> GenM ()
forbidVoid (Void pos) = failPos pos $ "Type error, variables cannot have void type"
forbidVoid _ = return ()

checkEqual :: Type Pos -> Type Pos -> GenM ()
checkEqual ty expTy =
  unless ((plainType ty) == (plainType expTy)) $ failPos pos $
    "Type error, got: " ++ show expTy ++ ", expected: " ++ show ty
      where
        pos = getTypePos ty


------------------------ Analysis --------------------------------------------
analyzeProgram :: Program Pos -> GenM (Program Pos)
analyzeProgram (Program pos topDefs) = do
  buildTopEnv topDefs
  checkMain
  newTopDefs <- mapM analyzeTopDef topDefs
  return $ Program pos newTopDefs


analyzeTopDef :: TopDef Pos -> GenM (TopDef Pos)
analyzeTopDef (FnDef pos ty ident@(Ident i) args block) = do
  startNewFun ident ty
  analyzeArgs args
  -- TODO set outer env

  (newBlock, endsRet) <- analyzeBlock block

  unless endsRet $ fail $ "Error: no return instruction in function " ++ show i

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
  (newExpr, expTy) <- analyzeExpr expr
  checkEqual ty expTy
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
  (newExpr, expTy) <- analyzeExpr expr
  -- TODO type?
  return (SExp pos newExpr, False)

analyzeStmt (Ass pos ident expr) = do
  (newExpr, expTy) <- analyzeExpr expr
  ty <- getIdentType ident
  checkEqual ty expTy
  return (Ass pos ident newExpr, False)

analyzeStmt (Incr pos ident) = do
  ty <- getIdentType ident
  checkEqual (Int Nothing) ty
  let identPlus1 = EAdd pos (EVar pos ident) (Plus pos) (ELitInt pos 1)
  return (Ass pos ident identPlus1, False)

analyzeStmt (Decr pos ident) = do
  ty <- getIdentType ident
  checkEqual (Int Nothing) ty
  let identMinus1 = EAdd pos (EVar pos ident) (Minus pos) (ELitInt pos 1)
  return (Ass pos ident identMinus1, False)

analyzeStmt (Cond pos cond thenStmt) = do
  newCond <- analyzeCond cond
  (newThen, endsRetThen) <- analyzeStmt thenStmt
  -- TODO check if there was a return already
  return $ case newCond of
    ELitTrue _ -> (newThen, endsRetThen)
    ELitFalse pos -> (Empty pos, False)
    _ -> (Cond pos newCond newThen, False)

analyzeStmt (CondElse pos cond thenStmt elseStmt) = do
  newCond <- analyzeCond cond
  (newThen, endsRetThen) <- analyzeStmt thenStmt
  (newElse, endsRetElse) <- analyzeStmt elseStmt
  -- TODO check if there was a return already
  return $ case newCond of
    ELitTrue _ -> (newThen, endsRetThen)
    ELitFalse _ -> (newElse, endsRetElse)
    _ -> (CondElse pos newCond newThen newElse, endsRetThen && endsRetElse)

analyzeStmt (While pos cond bodyStmt) = do
  newCond <- analyzeCond cond
  (newBody, endsRetBody) <- analyzeStmt bodyStmt
  return $ case newCond of
    ELitTrue _ -> (While pos newCond newBody, endsRetBody)
    ELitFalse _ -> (Empty pos, False)
    _ -> (While pos newCond newBody, False)

analyzeStmt (Ret pos expr) = do
  (newExpr, expTy) <- analyzeExpr expr
  retTy <- gets currentFunRetType
  checkEqual retTy expTy
  return (Ret pos newExpr, True)

analyzeStmt (VRet pos) = do
  retTy <- gets currentFunRetType
  checkEqual retTy (Void Nothing)
  return (VRet pos, True)


analyzeCond :: Expr Pos -> GenM (Expr Pos)
analyzeCond cond = do
  (newExpr, expTy) <- analyzeExpr cond
  checkEqual (Bool Nothing) expTy
  return newExpr


analyzeExpr :: Expr Pos -> GenM (Expr Pos, Type Pos)
analyzeExpr e@(EVar _ ident) = do
  ty <- getIdentType ident
  return (e, ty)

analyzeExpr e@(ELitInt pos _) = do
  return (e, Int pos)

analyzeExpr e@(ELitTrue pos) = do
  return (e, Bool pos)

analyzeExpr e@(ELitFalse pos) = do
  return (e, Bool pos)

analyzeExpr (EApp pos ident@(Ident i) exprs) = do
  funTy <- getIdentType ident
  (retTy, argsTys) <- case funTy of
    (Fun _ retTy argsTys) -> return (retTy, argsTys)
    _ -> failPos pos $ show i ++ "is not a function to apply"

  (newExprs, exprTys) <- liftM unzip $ mapM analyzeExpr exprs
  unless (length argsTys == length exprTys) $ failPos pos $
    "Wrong number of arguments to apply to function " ++ show i
  -- check arguments type equality
  mapM_ (uncurry checkEqual) $ zip argsTys exprTys
  return (EApp pos ident newExprs, retTy)

analyzeExpr e@(EString pos _) = do
  return (e, Str pos)

analyzeExpr (Neg pos expr) = do
  (newExpr, expTy) <- analyzeExpr expr
  checkEqual (Int Nothing) expTy
  return (Neg pos newExpr, expTy)

analyzeExpr (Not pos expr) = do
  (newExpr, expTy) <- analyzeExpr expr
  checkEqual (Bool Nothing) expTy
  return (Not pos newExpr, expTy)

analyzeExpr (EMul pos expr op expr2) = do
  (newExpr, expTy) <- analyzeExpr expr
  (newExpr2, expTy2) <- analyzeExpr expr2
  checkEqual (Int Nothing) expTy
  checkEqual (Int Nothing) expTy2
  return (EMul pos newExpr op newExpr2, Int Nothing)

analyzeExpr (EAdd pos expr op expr2) = do
  (newExpr, expTy) <- analyzeExpr expr
  (newExpr2, expTy2) <- analyzeExpr expr2
  checkEqual (Int Nothing) expTy
  checkEqual (Int Nothing) expTy2
  return (EAdd pos newExpr op newExpr2, Int Nothing)

analyzeExpr (ERel pos expr op expr2) = do
  (newExpr, expTy) <- analyzeExpr expr
  (newExpr2, expTy2) <- analyzeExpr expr2
  checkEqual (Int Nothing) expTy
  checkEqual (Int Nothing) expTy2
  return (ERel pos newExpr op newExpr2, Bool pos)

analyzeExpr (EAnd pos expr expr2) = do
  (newExpr, expTy) <- analyzeExpr expr
  (newExpr2, expTy2) <- analyzeExpr expr2
  checkEqual (Bool Nothing) expTy
  checkEqual (Bool Nothing) expTy2
  return (EAnd pos newExpr newExpr2, Bool Nothing)

analyzeExpr (EOr pos expr expr2) = do
  (newExpr, expTy) <- analyzeExpr expr
  (newExpr2, expTy2) <- analyzeExpr expr2
  checkEqual (Bool Nothing) expTy
  checkEqual (Bool Nothing) expTy2
  return (EOr pos newExpr newExpr2, Bool Nothing)
