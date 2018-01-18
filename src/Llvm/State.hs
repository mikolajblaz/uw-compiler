module Llvm.State where

import Control.Monad ( foldM )
import Control.Monad.Trans.State.Lazy
import Data.List ( nub )
import qualified Data.Map as Map

import Llvm.Core

import AbsLatte
import ErrM


-------------------------- Compiler state --------------------------------
type Counter = Integer

-- | A monad to run compiler in
type GenM = StateT GenState Err

emptyEnv :: IdentEnv
emptyEnv = Map.empty

initState :: GenState
initState = GenSt (Map.empty, Map.empty, Map.empty) (0, 0, 0)
              (0, [], Map.empty, [], Map.empty) (Void Nothing) []

-- | Compiler state
data GenState = GenSt {
  -- blockEnv, outerEnv, topEnv, see below
  envs :: (IdentEnv, IdentEnv, IdentEnv),
  -- identCnt, regCnt, labelCnt, see below
  counters :: (Counter, Counter, Counter),

  -- Blocks:
    -- currBlock :: Label,
    -- blockBuilder :: [String], -- current block output
    -- simpleBlocks :: Map.Map Label SBlock,
    -- succBuilder :: [Label] -- current block successors, not used now
    -- successors :: Map.Map Label [Label] -- not used now
  blocksDesc :: (Label, [String], Map.Map Label SBlock, [Label], Map.Map Label [Label]),

  currentFunRetType :: Type Pos,
  sConsts :: [StringConst] -- list of string constants
}
  deriving (Show)

-- GenSt envs cnts blocksDesc ret consts

first, second, third :: (a, a, a) -> a
first (a, _, _) = a
second (_, b, _) = b
third (_, _, c) = c

blockEnv, outerEnv, topEnv :: GenState -> IdentEnv
blockEnv = first  . envs
outerEnv = second . envs
topEnv   = third  . envs

identCnt, regCnt, labelCnt :: GenState -> Counter
identCnt = first  . counters
regCnt   = second . counters
labelCnt = third  . counters

currBlock :: GenState -> Label
currBlock    = (\(a, _, _, _, _) -> a) . blocksDesc
blockBuilder :: GenState -> [String] -- current block output
blockBuilder = (\(_, b, _, _, _) -> b) . blocksDesc
simpleBlocks :: GenState -> Map.Map Label SBlock
simpleBlocks = (\(_, _, c, _, _) -> c) . blocksDesc
succBuilder :: GenState -> [Label] -- current block successors
succBuilder  = (\(_, _, _, d, _) -> d) . blocksDesc
successors :: GenState -> Map.Map Label [Label]
successors   = (\(_, _, _, _, e) -> e) . blocksDesc

---------------------------- Helpers --------------------------------------
getArgType :: Arg a -> Type a
getArgType (Arg _ t _) = t

getTypePos :: Type Pos -> Pos
getTypePos typos = case typos of
  Int pos -> pos
  Str pos -> pos
  Bool pos -> pos
  Void pos -> pos
  Arr pos _ -> pos
  Fun pos _ _ -> pos

-- Counters
incIdentCnt :: GenState -> (Counter, GenState)
incIdentCnt (GenSt envs (iCnt, rCnt, lCnt) blocksDesc ret consts) =
  (iCnt, GenSt envs (iCnt + 1, rCnt, lCnt) blocksDesc ret consts)

incRegCnt :: GenState -> (Counter, GenState)
incRegCnt (GenSt envs (iCnt, rCnt, lCnt) blocksDesc ret consts) =
  (rCnt, GenSt envs (iCnt, rCnt + 1, lCnt) blocksDesc ret consts)

incLabelCnt :: GenState -> (Counter, GenState)
incLabelCnt (GenSt envs (iCnt, rCnt, lCnt) blocksDesc ret consts) =
  (lCnt, GenSt envs (iCnt, rCnt, lCnt + 1) blocksDesc ret consts)

addInstr :: Instr -> GenState -> GenState
addInstr instr (GenSt envs cnts (curr, instrs, blocks, succB, succs) ret consts) =
  GenSt envs cnts (curr, (instr : instrs), blocks, succB, succs) ret consts

setBlock :: Label -> GenState -> GenState
setBlock label (GenSt envs cnts (_, instrs, blocks, succB, succs) ret consts) =
  GenSt envs cnts (label, instrs, blocks, succB, succs) ret consts
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
  GenSt envs cnts (curr, instrs, blocks, succB, succs) ret consts <- get
  let newBlocks = Map.insert curr (reverse instrs) blocks
  let newSuccs = Map.insert curr (nub succB) succs -- unique labels
  put $ GenSt envs cnts (curr, [], newBlocks, succB, succs) ret consts

---------------------------- Functions ------------------------------------
-- TopEnv
startNewFun :: Ident -> Type Pos -> GenM ()
startNewFun ident ty = do
  GenSt (_, _, tEnv) (iCnt, _, _) _ _ sConsts <- get
  put $ initState
  setTopOuterEnv tEnv
  modify (\(GenSt envs (_, rCnt, lCnt) blocks _  _) ->
    GenSt envs (iCnt, rCnt, lCnt) blocks ty sConsts)

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
setNewEnvs blockEnv outerEnv = modify (\(GenSt (_, _, tEnv) cnts blocks ret consts) ->
    GenSt (blockEnv, outerEnv, tEnv) cnts blocks ret consts)


setNewEnv :: IdentEnv -> GenM ()
setNewEnv blockEnv = modify (\(GenSt (_, oEnv, tEnv) cnts blocks ret consts) ->
        GenSt (blockEnv, oEnv, tEnv) cnts blocks ret consts)

setTopOuterEnv :: IdentEnv -> GenM ()
setTopOuterEnv tEnv = modify (\(GenSt (bEnv, _, _) cnts blocks ret consts) ->
            GenSt (bEnv, tEnv, tEnv) cnts blocks ret consts)


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

createStringConstant :: String -> GenM Addr
createStringConstant str = do
  uniqueIdent <- freshIdent emptyStringIdent
  let ty = TStrConst $ toInteger $ length str + 1
  let addr = AStr uniqueIdent ty
  addStringConstant $ SConst str addr
  return addr

addStringConstant :: StringConst -> GenM ()
addStringConstant sc = modify (\(GenSt envs cnts blocks ret consts) ->
      GenSt envs cnts blocks ret (sc : consts))

-- special identifier, cannot be created by a user
emptyStringIdent :: Ident
emptyStringIdent = Ident ""
