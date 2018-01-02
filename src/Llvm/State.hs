module Llvm.State where

import Control.Monad ( foldM, liftM )
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import Data.Maybe

import Llvm.Core

import AbsLatte
import ErrM


type TypeEnv = Map.Map Ident (Type Pos)

-------------------------- Frontend state --------------------------------
-- | Context analysis (frontend) state
data GenState = GenSt {
  blockEnv :: TypeEnv,
  outerEnv :: TypeEnv,
  topEnv :: TypeEnv,
  output :: [String]
}
  deriving (Show)

-- | A monad to run frontend in
type GenM = StateT GenState Err

emptyEnv = Map.empty

initState :: TypeEnv -> GenState
initState topEnv = GenSt Map.empty Map.empty topEnv []


-- ----------------------- Operations on environment ----------------------

-- Inside GenM
startNewFun :: Type Pos -> GenM ()
startNewFun = undefined

setNewEnvs :: TypeEnv -> TypeEnv -> GenM ()
setNewEnvs blockEnv outerEnv = modify setNew
  where
    setNew (GenSt be oe te out) = GenSt blockEnv outerEnv te out


-- Outside GenM
-- | Insert block environment to outer environment.
blockToOuterEnv :: TypeEnv -> TypeEnv -> TypeEnv
blockToOuterEnv blockEnv outerEnv = Map.union outerEnv blockEnv





----------------------------- Blocks ---------------------------------------
getNewBlockLabel :: GenM SBlockLabel
getNewBlockLabel = undefined

{-|
getAddr :: Ident -> GenM Addr
getAddr ident = do
  env <- gets env
  case Map.lookup ident env of
    Just a -> return a
    Nothing -> fail $ "Undefined variable " ++ show ident

-- return new address and Bool inidicating if it was newly allocated
getAddrOrAlloc :: Ident -> GenM (Addr, Bool)
getAddrOrAlloc ident = do
  env <- gets env
  case Map.lookup ident env of
    Just a -> return (a, False)
    Nothing -> do {
      a <- freshLocal;
      St env l r o <- get;
      put $ St (Map.insert ident a env) l r o;
      return (a, True)
    }

freshLocal :: GenM Addr
freshLocal = liftM Loc $ state incCounter
  where
    incCounter (St env l r o) = (l, St env (l + 1) r o)

freshRegister :: GenM Addr
freshRegister = liftM Reg $ state incCounter
  where
    incCounter (St env l r o) = (r, St env l (r + 1) o)

getLocalsCount :: GenM Integer
getLocalsCount = do
  locals <- gets locals
  return $ locals

|-}
