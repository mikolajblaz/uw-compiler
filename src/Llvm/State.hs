module Llvm.State where

import Control.Monad ( foldM, liftM )
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import Data.Maybe

import Llvm.Core

import AbsLatte
import ErrM


-------------------------- Frontend state --------------------------------
-- | Context analysis (frontend) state
data FrontEnv a = FrontEnv {
  identTypes :: Map.Map Ident (Type a)
}
  deriving (Show)

data FrontState a = FrontSt {
  env :: FrontEnv a
}
  deriving (Show)

-- | A monad to run frontend in
type FrontM a = StateT (FrontState a) Err

emptyFrontState :: FrontState a
emptyFrontState = FrontSt (FrontEnv Map.empty)


-------------------------- Backend state ---------------------------------
{-|
type Counter = Integer
type IdentEnv = Map.Map Ident Addr
-- | Whole environment
data GenState = St {
  env :: IdentEnv,
  locals :: Counter,
  registers :: Counter,       -- not used in JVM
  output :: [Instruction]     -- not used in JVM
}
  deriving (Show)

-- | A monad to run backend in
type GenM = StateT GenState Err

-- | Create empty environment
emptyState :: GenState
emptyState = St Map.empty 1 0 []     -- start numerating locals from 1



-- ----------------------- Operations on environment ----------------------

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
