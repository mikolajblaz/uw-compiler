module Llvm.Frontend where

import Control.Monad ( foldM, liftM )
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import Data.Maybe

import Llvm.Core

import AbsLatte
import ErrM

runContextAnalysis :: Program Pos -> Err String -- TODO type
runContextAnalysis program = do
  topEnv <- buildTopEnv program
  return $ "OK"


---------------------------------------------------------------------

type TopEnv a = Map.Map Ident (Type a)

getArgType :: Arg a -> Type a
getArgType (Arg _ t _) = t

-- | Build data environment basing on 'data' declarations
buildTopEnv :: Program Pos -> Err (TopEnv Pos)
buildTopEnv (Program _ defs) = do
  foldM (flip insertTopDef) Map.empty defs

insertTopDef :: TopDef Pos -> (TopEnv Pos) -> Err (TopEnv Pos)
insertTopDef (FnDef pos ty ident@(Ident i) args _) topEnv = case Map.lookup ident topEnv of
  -- Just _ -> failPos pos $ "Function " ++ show ident ++ " already declared"
  Just _ -> failPos pos $ "Function " ++ show i ++ " already declared"
  Nothing -> return $ Map.insert ident funType topEnv
    where
      funType = Fun pos ty (map getArgType args)

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
