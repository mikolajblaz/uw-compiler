module Llvm.Frontend (analyzeProgram) where

import Control.Monad ( liftM, unless )
import Control.Monad.Trans.State.Lazy
import Data.List ( partition )
import qualified Data.Map as Map

import Llvm.Core
import Llvm.State
import qualified Llvm.StdLib as Lib

import AbsLatte

----------------------- Main function  ---------------------------------------
-- check if main exists and check type
checkMain :: GenM ()
checkMain = do
  topEnv <- gets topEnv
  mainType <- case Map.lookup (Ident "main") topEnv of
    Nothing -> fail $ "Error: No 'main' function defined."
    Just (ty, _, _) -> return ty

  unless ((plainType mainType) == (TFun TInt [])) $ failPos (getTypePos mainType) $
    "Type error: the 'main' function must have a signature 'int main ()'"


----------------------- Type check -------------------------------------------
forbidVoid :: Type Pos -> GenM ()
forbidVoid (Void pos) = failPos pos $ "Type error: variables cannot have void type"
forbidVoid _ = return ()

forbidVoidFunComparison :: Type Pos -> GenM ()
forbidVoidFunComparison (Void pos) = failPos pos "Type Error: Cannot compare 'void' type"
forbidVoidFunComparison (Fun pos _ _) = failPos pos "Type Error: Cannot compare function type"
forbidVoidFunComparison _ = return ()

checkEqual :: Pos -> Type Pos -> Type Pos -> GenM ()
checkEqual pos expTy ty =
  unless ((plainType ty) == (plainType expTy)) $ failPos pos $
    "Type error: got: " ++ printLatte ty ++ ", expected: " ++ printLatte expTy


checkArrayType :: Pos -> Type Pos -> GenM (Type Pos)
checkArrayType pos arrTy = case arrTy of
                Arr _ ty -> return ty
                ty -> failPos pos $ "Cannot extract array element from type " ++ show ty

checkTypeExists :: Type Pos -> GenM ()
checkTypeExists (Cls pos ident@(Ident i)) = do
  clsEnv <- gets classEnv
  case Map.lookup ident clsEnv of
    Nothing -> failPos pos $ "Type " ++ i ++ " does not exist"
    _ -> return ()
checkTypeExists (Arr _ ty) = checkTypeExists ty
checkTypeExists _ = return () -- other types just exist

------------------------ Analysis --------------------------------------------
analyzeProgram :: Program Pos -> GenM (Program Pos)
analyzeProgram (Program pos topDefs) = do
  let (fnDefs, clsDefs) = partition isFnDef topDefs
  buildTopEnv (Lib.libraryTopDefs ++ fnDefs)
  buildClassEnv clsDefs
  checkMain
  -- NOTE: we don't analyze libraryTopDefs, we just put it to topEnv
  newTopDefs <- mapM analyzeTopDef topDefs
  return $ Program pos newTopDefs


analyzeTopDef :: TopDef Pos -> GenM (TopDef Pos)
analyzeTopDef (FnDef pos ty ident@(Ident i) args block) = do
  checkTypeExists ty
  startNewFun ident ty
  analyzeArgs args
  -- NOTE: now, localEnv is set, it will be merged with outerEnv in the following call:
  (Block pos newStmts, endsRet) <- analyzeBlock block

  unless (endsRet || (plainType ty == TVoid)) $
    fail $ "Error: missing return statement in function " ++ show i ++ " (declared in " ++ printPos pos ++ ")"

  -- append return if not present
  let finalStmts = if endsRet
      then newStmts
      else newStmts ++ [VRet Nothing]

  return $ FnDef pos ty ident args (Block pos finalStmts)

analyzeTopDef d = return d


analyzeArgs :: [Arg Pos] -> GenM ()
analyzeArgs args = mapM_ analyzeArg args

analyzeArg :: Arg Pos -> GenM ()
analyzeArg (Arg pos ty ident) = do
  forbidVoid ty
  checkTypeExists ty
  _ <- insertLocalDecl ident ty
  return ()

-- Return possibly changed abstract tree and a flag inidicating whether
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
  -- Set old environment back, discarding everything that was inside the block
  setNewEnvs oldBlockEnv oldOuterEnv

  -- trim instructions after ret
  let (noRets, afterFirstRet) = break id endsRets
  -- if any instruction ends with ret, the block ends with ret also
  let endsRet = not $ null afterFirstRet
  let trimmedStmts = if endsRet
                       then take (length noRets + 1) newStmts
                       else newStmts -- no change
  return $ (Block pos trimmedStmts, endsRet)

  -- Return possibly change abstract tree and flag inidicating whether
  -- statment ends with a return
analyzeStmt :: Stmt Pos -> GenM (Stmt Pos, Bool)
analyzeStmt (BStmt pos b) = do
  (newB, endsRet) <- analyzeBlock b
  return $ (BStmt pos newB, endsRet)

-- analyze single declaration
analyzeStmt d@(Decl pos ty [NoInit pos2 ident]) = do
  forbidVoid ty
  checkTypeExists ty
  _ <- insertLocalDecl ident ty
  return (d, False)

analyzeStmt (Decl pos ty [Init pos2 ident expr]) = do
  forbidVoid ty
  checkTypeExists ty
  (newExpr, exprTy) <- analyzeExpr expr
  checkEqual pos2 ty exprTy
  _ <- insertLocalDecl ident ty
  return (Decl pos ty [Init pos2 ident newExpr], False)

-- multiple declarations
analyzeStmt (Decl pos ty items) = do
  let singleDecls = map (\item -> Decl pos ty [item]) items
  declsList <- mapM analyzeStmt singleDecls
  return $ (Decl pos ty $ map extractInit declsList, False)
    where
      extractInit (Decl _ _ [initItem], _) = initItem
      extractInit _ = undefined -- this should not happen


analyzeStmt (Empty pos) = return (Empty pos, False)

analyzeStmt (SExp pos expr) = do
  case expr of
    (EApp _ _ _) -> return ()
    _ -> failPos pos $ printTreeOneLine expr ++ "is not a statement"
  (newExpr, exprTy) <- analyzeExpr expr
  return (SExp pos newExpr, False)

analyzeStmt (Ass pos lhsExpr expr) = do
  (newExpr, exprTy) <- analyzeExpr expr
  lhsTy <- analyzeLhs lhsExpr
  checkEqual (getExprPos expr) lhsTy exprTy
  return (Ass pos lhsExpr newExpr, False)

analyzeStmt (Incr pos lhsExpr) = do
  lhsTy <- analyzeLhs lhsExpr
  checkEqual pos (Int Nothing) lhsTy
  let identPlus1 = EAdd pos lhsExpr (Plus pos) (ELitInt pos 1)
  return (Ass pos lhsExpr identPlus1, False)

analyzeStmt (Decr pos lhsExpr) = do
  lhsTy <- analyzeLhs lhsExpr
  checkEqual pos (Int Nothing) lhsTy
  let identMinus1 = EAdd pos lhsExpr (Minus pos) (ELitInt pos 1)
  return (Ass pos lhsExpr identMinus1, False)

analyzeStmt (Cond pos cond thenStmt) = do
  newCond <- analyzeCond cond
  (newThen, endsRetThen) <- analyzeStmt thenStmt
  return $ case newCond of
    ELitTrue _ -> (newThen, endsRetThen)
    ELitFalse pos -> (Empty pos, False)
    _ -> (Cond pos newCond newThen, False)

analyzeStmt (CondElse pos cond thenStmt elseStmt) = do
  newCond <- analyzeCond cond
  (newThen, endsRetThen) <- analyzeStmt thenStmt
  (newElse, endsRetElse) <- analyzeStmt elseStmt
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

analyzeStmt (For pos ty ident arrExpr stmt) = do
  (newArrExpr, arrTy) <- analyzeExpr arrExpr
  elemTy <- checkArrayType pos arrTy
  checkEqual pos elemTy ty

  let index = Ident "_i"    -- '_i' is not a valid Latte variable name, so no conflict
  let array = Ident "_arr"  -- '_arr' is not a valid Latte variable name, so no conflict
  let whileCond = ERel pos (EVar pos index) (LTH pos) (EFieldAcc pos (EVar pos array) (EVar pos (Ident "length")))
  let forAsWhile = BStmt pos $ Block pos [
                      Decl pos arrTy [Init pos array newArrExpr],
                      Decl pos (Int pos) [NoInit pos index],
                      Decl pos ty [NoInit pos ident],
                      While pos whileCond $ BStmt pos $ Block pos [
                          Ass pos (EVar pos ident) (EArrayAcc pos (EVar pos array) (EVar pos index)),
                          stmt,
                          Incr pos (EVar pos index)
                        ]
                    ]

  (newFor, _) <- analyzeStmt forAsWhile
  return (newFor, False)

analyzeStmt (Ret pos expr) = do
  (newExpr, exprTy) <- analyzeExpr expr
  retTy <- gets currentFunRetType
  checkEqual pos retTy exprTy
  return (Ret pos newExpr, True)

analyzeStmt (VRet pos) = do
  retTy <- gets currentFunRetType
  checkEqual pos retTy (Void Nothing)
  return (VRet pos, True)


analyzeCond :: Expr Pos -> GenM (Expr Pos)
analyzeCond cond = do
  (newExpr, exprTy) <- analyzeExpr cond
  checkEqual (getExprPos cond) (Bool Nothing) exprTy
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
    "Wrong number of arguments to apply to function " ++ show i ++
      ", got: " ++ show (length exprTys) ++ ", expected: " ++ show (length argsTys) ++ " arguments"
  -- check arguments type equality
  mapM_ (\(eTy, aTy, poss) -> checkEqual poss aTy eTy) $ zip3 exprTys argsTys (map getExprPos exprs)
  return (EApp pos ident newExprs, retTy)

analyzeExpr (EString pos str) = do
  -- strip quotes added by BNFC
  let noQuotes = tail $ init str
  return (EString pos noQuotes, Str pos)

analyzeExpr (Neg pos expr) = do
  (newExpr, exprTy) <- analyzeExpr expr
  checkEqual pos (Int Nothing) exprTy
  return (Neg pos newExpr, exprTy)

analyzeExpr (Not pos expr) = do
  (newExpr, exprTy) <- analyzeExpr expr
  checkEqual pos (Bool Nothing) exprTy
  let finalExpr = case newExpr of
                    ELitTrue pos -> ELitFalse pos
                    ELitFalse pos -> ELitTrue pos
                    e -> Not pos e
  return (finalExpr, exprTy)

analyzeExpr (EMul pos expr op expr2) = do
  (newExpr, exprTy) <- analyzeExpr expr
  (newExpr2, exprTy2) <- analyzeExpr expr2
  checkEqual (getExprPos expr) (Int Nothing) exprTy
  checkEqual (getExprPos expr2) (Int Nothing) exprTy2
  return (EMul pos newExpr op newExpr2, Int pos)

analyzeExpr (EAdd pos expr op expr2) = do
  (newExpr, exprTy) <- analyzeExpr expr
  (newExpr2, exprTy2) <- analyzeExpr expr2
  case op of
    Minus _ -> checkEqual (getExprPos expr) exprTy (Int Nothing)
    Plus _ -> case exprTy of
      Int _ -> return ()
      Str _ -> return ()
      _ -> failPos pos $ "Expected int or string, got: " ++ printLatte exprTy

  checkEqual (getExprPos expr2) exprTy exprTy2
  return (EAdd pos newExpr op newExpr2, exprTy)

analyzeExpr (ERel pos expr rel expr2) = do
  (newExpr, exprTy) <- analyzeExpr expr
  (newExpr2, exprTy2) <- analyzeExpr expr2
  case rel of
      EQU _ -> forbidVoidFunComparison exprTy
      NE _ -> forbidVoidFunComparison exprTy
      _ -> case exprTy of
            Int _ -> return ()
            Bool _ -> return ()
            _ -> failPos (getExprPos expr) $
              "Inequality can be applied only to boolean and int type, not: " ++ printLatte exprTy

  unless ((plainType exprTy) == (plainType exprTy2)) $ failPos (getExprPos expr) $
    "Type error: compared types must be equal"
  return (ERel pos newExpr rel newExpr2, Bool pos)

analyzeExpr (EAnd pos expr expr2) = do
  (newExpr, exprTy) <- analyzeExpr expr
  (newExpr2, exprTy2) <- analyzeExpr expr2
  checkEqual (getExprPos expr) (Bool Nothing) exprTy
  checkEqual (getExprPos expr2) (Bool Nothing) exprTy2
  -- lazy evaluation:
  let finalExpr = case newExpr of
                    ELitTrue _ -> newExpr2
                    ELitFalse _ -> ELitFalse pos
                    e -> EAnd pos e newExpr2
  return (finalExpr, Bool pos)

analyzeExpr (EOr pos expr expr2) = do
  (newExpr, exprTy) <- analyzeExpr expr
  (newExpr2, exprTy2) <- analyzeExpr expr2
  checkEqual (getExprPos expr) (Bool Nothing) exprTy
  checkEqual (getExprPos expr2) (Bool Nothing) exprTy2
  let finalExpr = case newExpr of
                    ELitTrue _ -> ELitTrue pos
                    ELitFalse _ -> newExpr2
                    e -> EOr pos e newExpr2
  return (finalExpr, Bool pos)

analyzeExpr (ENewArray pos ty sizeExpr) = do
  (newSizeExpr, sizeTy) <- analyzeExpr sizeExpr
  checkEqual (getExprPos sizeExpr) (Int Nothing) sizeTy
  return (ENewArray pos ty newSizeExpr, (Arr (getTypePos ty) ty))

analyzeExpr (EArrayAcc pos arrExpr iExpr) = do
  (newArrExpr, arrTy) <- analyzeExpr arrExpr
  elemTy <- checkArrayType pos arrTy

  (newIndexExpr, indexTy) <- analyzeExpr iExpr
  checkEqual (getExprPos iExpr) (Int Nothing) indexTy

  return (EArrayAcc pos newArrExpr newIndexExpr, elemTy)


analyzeExpr (EFieldAcc pos expr fieldExpr) = do
  (newExpr, exprTy) <- analyzeExpr expr
  fieldTy <- analyzeFieldExpr exprTy fieldExpr
  return (EFieldAcc pos newExpr fieldExpr, fieldTy)

analyzeExpr e@(ENull _ ty) = return (e, ty)


analyzeFieldExpr :: Type Pos -> Expr Pos -> GenM (Type Pos)
analyzeFieldExpr (Arr pos _) fieldExpr = do
  case fieldExpr of
    EVar _ (Ident "length") -> return $ Int pos
    EVar p _ -> failPos p $ "Arrays have only one field 'length'"
    e -> failPos (getExprPos e) $ "Not a field expression"

analyzeFieldExpr _ _ = fail "Structs not implemented"

-- Lhs
analyzeLhs :: Expr Pos -> GenM (Type Pos)
analyzeLhs (EVar pos ident) = getIdentType pos ident

analyzeLhs (EArrayAcc pos arrExpr iExpr) = do
  arrTy <- analyzeLhs arrExpr
  checkArrayType pos arrTy

-- TODO Field Acces
analyzeLhs expr = failPos (getExprPos expr) $ "Invalid left-hand-side expression."


-- Helper
getExprPos :: Expr Pos -> Pos
getExprPos (ENewArray pos _ _) = pos
getExprPos (EArrayAcc pos _ _) = pos
getExprPos (EFieldAcc pos _ _) = pos
getExprPos (ENull pos _) = pos
getExprPos (EVar pos _) = pos
getExprPos (ELitInt pos _) = pos
getExprPos (ELitTrue pos) = pos
getExprPos (ELitFalse pos) = pos
getExprPos (EApp pos _ _) = pos
getExprPos (EString pos _) = pos
getExprPos (Neg pos _) = pos
getExprPos (Not pos _) = pos
getExprPos (EMul pos _ _ _) = pos
getExprPos (EAdd pos _ _ _) = pos
getExprPos (ERel pos _ _ _) = pos
getExprPos (EAnd pos _ _) = pos
getExprPos (EOr pos _ _) = pos
