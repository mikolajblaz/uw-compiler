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

  -- counters
  identCnt :: Counter,
  regCnt :: Counter,
  labelCnt :: Counter,

  -- Blocks
  currBlock :: Label,
  blockBuilder :: [String], -- current block output
  simpleBlocks :: Map.Map Label SBlock,
  -- TODO block predecessors?

  currentFun :: Ident,
  currentFunRetType :: Type Pos,
  funBlocks :: Map.Map Ident [Label]  --TODO remove
}
  deriving (Show)

-- GenSt bEnv oEnv tEnv iCnt rCnt lCnt cB bB sB fun fty funB

-- | A monad to run compiler in
type GenM = StateT GenState Err

emptyEnv :: IdentEnv
emptyEnv = Map.empty

initState :: GenState
initState = GenSt Map.empty Map.empty Map.empty 0 0 0 0 [] Map.empty (Ident "") (Void Nothing) Map.empty

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
incIdentCnt (GenSt bEnv oEnv tEnv iCnt rCnt lCnt cB bB sB fun fty funB) =
  (iCnt, GenSt bEnv oEnv tEnv (iCnt + 1) rCnt lCnt cB bB sB fun fty funB)

incRegCnt :: GenState -> (Counter, GenState)
incRegCnt (GenSt bEnv oEnv tEnv iCnt rCnt lCnt cB bB sB fun fty funB) =
  (rCnt, GenSt bEnv oEnv tEnv iCnt (rCnt + 1) lCnt cB bB sB fun fty funB)

incLabelCnt :: GenState -> (Counter, GenState)
incLabelCnt (GenSt bEnv oEnv tEnv iCnt rCnt lCnt cB bB sB fun fty funB) =
  (lCnt, GenSt bEnv oEnv tEnv iCnt rCnt (lCnt + 1) cB bB sB fun fty funB)

addInstr :: Instr -> GenState -> GenState
addInstr instr (GenSt bEnv oEnv tEnv iCnt rCnt lCnt cB bB sB fun fty funB) =
  GenSt bEnv oEnv tEnv iCnt rCnt lCnt cB (instr : bB) sB fun fty funB

setBlock :: Label -> GenState -> GenState
setBlock label (GenSt bEnv oEnv tEnv iCnt rCnt lCnt _ bB sB fun fty funB) =
  GenSt bEnv oEnv tEnv iCnt rCnt lCnt label bB sB fun fty funB
-------------------------- Operations on state ----------------------

-- Inside GenM

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


getIdentVal :: Pos -> Ident -> GenM EnvVal
getIdentVal pos ident@(Ident i) = do
  bEnv <- gets blockEnv
  case Map.lookup ident bEnv of
    Just val -> return val
    Nothing -> do {
      oEnv <- gets outerEnv;
      case Map.lookup ident oEnv of
        Just val -> return val
        Nothing -> failPos pos $ "Undeclared variable: " ++ show i
    }

getIdentType :: Pos -> Ident -> GenM (Type Pos)
getIdentType pos ident = do
  (ty, _, _) <- getIdentVal pos ident
  return ty


finishBlock :: GenM ()
finishBlock = do
  GenSt lEnv oEnv tEnv iCnt rCnt lCnt currBlock blockBuilder simpleBlocks cFun fty funB <- get
  let newBlocks = Map.insert currBlock (reverse blockBuilder) simpleBlocks
  put $ GenSt lEnv oEnv tEnv iCnt rCnt lCnt currBlock [] newBlocks cFun fty funB

---------------------------- Functions ------------------------------------
-- TopEnv
startNewFun :: Ident -> Type Pos -> GenM ()
startNewFun ident ty = do
  GenSt _ _ tEnv iCnt _ _ _ _ _ _ _ _ <- get
  put $ initState
  setTopOuterEnv tEnv
  modify (\(GenSt bEnv oEnv tEnv _ rCnt lCnt cB bB sB _ _ funB) ->
    GenSt bEnv oEnv tEnv iCnt rCnt lCnt cB bB sB ident ty funB)

buildTopEnv :: [TopDef Pos] -> GenM ()
buildTopEnv defs = do
  topEnv <- foldM (flip insertTopDef) Map.empty defs
  setTopOuterEnv topEnv

insertTopDef :: TopDef Pos -> IdentEnv -> GenM IdentEnv
insertTopDef (FnDef pos ty ident args _) topEnv = do
  let funType = Fun pos ty (map getArgType args)
  insertUniqueNewIdent ident funType (AFun ident (plainType funType)) topEnv

------------------- Operations on identifiers environment -----------------

setNewEnvs :: IdentEnv -> IdentEnv -> GenM ()
setNewEnvs blockEnv outerEnv = modify (\(GenSt _ _ tEnv iCnt rCnt lCnt cB bB sB fun fty funB) ->
    GenSt blockEnv outerEnv tEnv iCnt rCnt lCnt cB bB sB fun fty funB)


setNewEnv :: IdentEnv -> GenM ()
setNewEnv blockEnv = modify (\(GenSt _ oEnv tEnv iCnt rCnt lCnt cB bB sB fun fty funB) ->
    GenSt blockEnv oEnv tEnv iCnt rCnt lCnt cB bB sB fun fty funB)

setTopOuterEnv :: IdentEnv -> GenM ()
setTopOuterEnv tEnv = modify (\(GenSt bEnv _ _ iCnt rCnt lCnt cB bB sB fun fty funB) ->
    GenSt bEnv tEnv tEnv iCnt rCnt lCnt cB bB sB fun fty funB)


-- Outside GenM
-- | Insert block environment to outer environment.
blockToOuterEnv :: IdentEnv -> IdentEnv -> IdentEnv
blockToOuterEnv blockEnv outerEnv = Map.union blockEnv outerEnv



insertUnique :: Ident -> EnvVal -> IdentEnv -> GenM IdentEnv
insertUnique ident@(Ident i) val@(ty, _, _) env = case Map.lookup ident env of
    Just _ -> failPos (getTypePos ty) $ "Variable " ++ i ++ " already declared"
    Nothing -> return $ Map.insert ident val env

insertUniqueNewIdent :: Ident -> (Type Pos) -> Addr -> IdentEnv -> GenM IdentEnv
insertUniqueNewIdent ident ty addr env = do
  uniqueIdent <- freshIdent ident
  insertUnique ident (ty, uniqueIdent, addr) env

insertLocalDecl :: Ident -> (Type Pos) -> GenM Addr
insertLocalDecl ident ty = do
  uniqueIdent <- freshIdent ident
  let identAddr = ALoc uniqueIdent $ plainType ty
  blockEnv <- gets blockEnv
  newBlockEnv <- insertUnique ident (ty, uniqueIdent, identAddr) blockEnv
  setNewEnv newBlockEnv
  return identAddr
