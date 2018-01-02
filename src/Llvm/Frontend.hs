module Llvm.Frontend where

import Control.Monad ( foldM, liftM, unless )
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import Data.Maybe

import Llvm.Core
import Llvm.State

import AbsLatte
import ErrM


updateEnv :: Stmt Pos -> Err (Type Pos)
updateEnv = undefined
-- sprawdź czy w blockEnv nie ma już tego identyfikatora
-- potem zasłoń
-- jeśli to jest BlockStmt, to mergeEnvs i wystartuj z pustym blockEnv


------------------ Build functions environment --------------------------------

getArgType :: Arg a -> Type a
getArgType (Arg _ t _) = t

-- | Build data environment basing on 'data' declarations
buildTopEnv :: [TopDef Pos] -> GenM ()
buildTopEnv defs = do
  topEnv <- foldM (flip insertTopDef) Map.empty defs
  -- nothing in state yet, so simply call initState
  put $ initState topEnv

insertTopDef :: TopDef Pos -> TypeEnv -> GenM TypeEnv
insertTopDef (FnDef pos ty ident@(Ident i) args _) topEnv = case Map.lookup ident topEnv of
  -- Just _ -> failPos pos $ "Function " ++ show ident ++ " already declared"
  Just _ -> failPos pos $ "Function " ++ show i ++ " already declared"
  Nothing -> return $ Map.insert ident funType topEnv
    where
      funType = Fun pos ty (map getArgType args)

----------------------- Main function  ---------------------------------------
checkMain :: GenM ()
checkMain = do
  topEnv <- gets topEnv
  -- check if main exists and check type
  -- TODO
  undefined


----------------------- Type check -------------------------------------------
checkTypeStmt :: Stmt Pos -> GenM (Type Pos)
checkTypeStmt = undefined

checkType :: Expr Pos -> GenM (Type Pos)
checkType = undefined

expectType :: Type Pos -> Expr Pos -> GenM ()
expectType ty exp = do
  expTy <- checkType exp
  unless (ty == expTy) $ failPos pos $
      "Type error, got: " ++ show expTy ++ ", expected: " ++ show ty
    where
      pos = getPosFromType ty

getPosFromType :: Type Pos -> Pos
getPosFromType = undefined


------------------------------------------------------------------------------
-- Returns
checkReturnEnding :: GenM ()
checkReturnEnding = undefined








------------------------------------------------------------------------------
-- staticTypeCheck :: Exp -> Env Type -> DataEnv -> Err Type
-- staticTypeCheck exp env de = runReaderT (checkType de exp) env
--
-- checkType :: DataEnv -> Exp -> TypeM Type
-- checkType de (If e1 e2 e3) = do
--   compareType de boolType e1
--   sameTypes de [e2, e3]
--
-- -- | Evaluate type of expression and check equality
-- compareType :: DataEnv -> Type -> Exp -> TypeM Type
-- compareType de t e = checkType de e >>= simpleCheck t
--
-- compareTypes :: DataEnv -> [Type] -> [Exp] -> TypeM ()
-- compareTypes de ts es = mapM_ (uncurry $ compareType de) $ zip ts es
--
-- -- | Simple type comparison
-- simpleCheck :: Type -> Type -> TypeM Type
-- simpleCheck tExpected tActual = if tExpected == tActual
--     then return tActual
--     else fail $ "TypeCheckError: Expected " ++ printTree tExpected ++ ", got " ++ printTree tActual
