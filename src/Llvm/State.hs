module Llvm.State where

import Control.Monad ( foldM )
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map

import Llvm.Core

import AbsLatte
import ErrM


-------------------------- Compiler state --------------------------------
type Counter = Integer

-- | Compiler state
data GenState = GenSt {
  blockEnv :: IdentEnv,
  outerEnv :: IdentEnv,
  topEnv :: IdentEnv,
  output :: [String],
  -- counters
  identCnt :: Counter
  -- regCnt :: Counter
  -- labelCnt :: Counter

  -- Blocks
  -- output :: [String] -- TODO currentBlockOutput

}
  deriving (Show)

-- | A monad to run compiler in
type GenM = StateT GenState Err

emptyEnv :: IdentEnv
emptyEnv = Map.empty

initState :: IdentEnv -> GenState
initState topEnv = GenSt Map.empty topEnv topEnv [] 0
-- TODO rethink putting topEnv to outerEnv (it's probably correct)



---------------------------- Helpers --------------------------------------
getArgType :: Arg a -> Type a
getArgType (Arg _ t _) = t

getTypePos :: Type Pos -> Pos
getTypePos typos = case typos of
  Int pos -> pos
  Str pos -> pos
  Bool pos -> pos
  Void pos -> pos
  Fun pos _ _ -> pos

-- Counters
incIdentCnt :: GenState -> (Counter, GenState)
incIdentCnt (GenSt be oe te out idc) = (idc, GenSt be oe te out (idc + 1))

incRegCnt :: GenState -> (Counter, GenState)
incRegCnt (GenSt be oe te out idc) = (regc, GenSt be oe te out idc) -- TODO
  where
    regc = idc

incLabelCnt :: GenState -> (Counter, GenState)
incLabelCnt (GenSt be oe te out idc) = (labc, GenSt be oe te out idc) -- TODO
  where
    labc = idc

addInstr :: Instr -> GenState -> GenState
addInstr instr (GenSt be oe te out idc) = GenSt be oe te (instr:out) idc

setBlock :: Label -> GenState -> GenState
setBlock l = id -- TODO
-------------------------- Operations on state ----------------------

-- Inside GenM
startNewFun :: Type Pos -> GenM ()
startNewFun ty = return () -- TODO

freshRegister :: TType -> GenM Addr
freshRegister ty = do
  regCnt <- state incRegCnt
  return $ AReg regCnt ty

-- Ident
freshIdent :: Ident -> GenM UniqueIdent
freshIdent (Ident ident) = do
  identCnt <- state incIdentCnt
  return $ UIdent ident identCnt

--  Blocks
freshLabel :: GenM Label
freshLabel = state incLabelCnt

setCurrentBlock :: Label -> GenM ()
setCurrentBlock label = modify (setBlock label)

-- TopEnv
buildTopEnv :: [TopDef Pos] -> GenM ()
buildTopEnv defs = do
  topEnv <- foldM (flip insertTopDef) Map.empty defs
  -- nothing in state yet, so simply call initState
  put $ initState topEnv

getIdentVal :: Ident -> GenM EnvVal
getIdentVal ident = do
  bEnv <- gets blockEnv
  case Map.lookup ident bEnv of
    Just val -> return val
    Nothing -> do {
      oEnv <- gets outerEnv;
      case Map.lookup ident oEnv of
        Just val -> return val
        Nothing -> fail $ "No such variable: " ++ show ident
    }

getIdentType :: Ident -> GenM (Type Pos)
getIdentType ident = do
  (ty, _, _) <- getIdentVal ident
  return ty


finishBlock :: GenM ()
finishBlock = return () -- TODO

------------------- Operations on identifiers environment -----------------

setNewEnvs :: IdentEnv -> IdentEnv -> GenM ()
setNewEnvs blockEnv outerEnv = modify setNew
  where
    setNew (GenSt _ _ te out idc) = GenSt blockEnv outerEnv te out idc

setNewEnv :: IdentEnv -> GenM ()
setNewEnv blockEnv = modify setNew
  where
    setNew (GenSt _ oe te out idc) = GenSt blockEnv oe te out idc


-- Outside GenM
-- | Insert block environment to outer environment.
blockToOuterEnv :: IdentEnv -> IdentEnv -> IdentEnv
blockToOuterEnv blockEnv outerEnv = Map.union outerEnv blockEnv



insertUnique :: Ident -> EnvVal -> IdentEnv -> GenM IdentEnv
insertUnique ident val@(ty, _, _) env = case Map.lookup ident env of
    Just _ -> failPos (getTypePos ty) $ show ident ++ " already declared"
    Nothing -> return $ Map.insert ident val env

insertUniqueNewIdent :: Ident -> (Type Pos) -> Addr -> IdentEnv -> GenM IdentEnv
insertUniqueNewIdent ident ty addr env = do
  uniqueIdent <- freshIdent ident
  insertUnique ident (ty, uniqueIdent, addr) env


insertTopDef :: TopDef Pos -> IdentEnv -> GenM IdentEnv
insertTopDef (FnDef pos ty ident args _) topEnv = do
  let funType = Fun pos ty (map getArgType args)
  insertUniqueNewIdent ident funType (AFun ident (plainType funType)) topEnv


insertLocalDecl :: Ident -> (Type Pos) -> GenM Addr
insertLocalDecl ident ty = do
  uniqueIdent <- freshIdent ident
  let identAddr = ALoc uniqueIdent $ plainType ty
  blockEnv <- gets blockEnv
  newBlockEnv <- insertUnique ident (ty, uniqueIdent, identAddr) blockEnv
  setNewEnv newBlockEnv
  return identAddr



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
