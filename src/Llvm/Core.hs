module Llvm.Core where

import Control.Monad
import qualified Data.Map as Map

import AbsLatte
import ErrM

--------------------------------- Basic types -------------------------------
type Pos = Maybe (Int, Int)

-- Simple blocks
-- NOTE: it's very different than Block from Latte
type SBlock = [Instr]

-- Block labels
type Label = Integer

-- address type
-- TODO include type information
data Addr =
    AImm Integer TType        -- Constant -- TODO with type?
  | AReg Integer TType        -- Temporary Registers
  | ALoc UniqueIdent TType    -- Local variables
  | AFun Ident TType          -- Functions (+ return type)
  | ALab Label                -- Block labels -- TODO needed?

instance Show Addr where    -- TODO needed?
  show _ = "ADDR"

  ------------------------- Identifiers --------------------------------

data UniqueIdent = UIdent String Integer
  deriving (Show)

type EnvVal = (Type Pos, UniqueIdent, Addr)
  -- | Identifiers environment
type IdentEnv = Map.Map Ident EnvVal

-------------------------- Instructions ----------------------------------
-- Plain text instructions
type Instr = String

-- Intermediate instructions (quadruple-code)
data QInstr =   -- TODO
    Store
  | Load
  deriving (Show)

-------------------------- Types -------------------------------------------
data TType = TInt | TStr | TBool | TVoid | TFun TType [TType] | TLab
  deriving (Eq)

instance Show TType where
  show TInt = "i32"
  show TStr = "i8*"
  show TBool = "i1"
  show TVoid = "void"
  show (TFun ty _) = show ty -- TODO ?
  show TLab = "label"

plainType :: Type Pos -> TType
plainType (Int _) = TInt
plainType (Str _) = TStr
plainType (Bool _) = TBool
plainType (Void _) = TVoid
plainType (Fun _ ty tys) = TFun (plainType ty) $ map plainType tys

-------------------------- Helpers -----------------------------------------
failPos :: (Monad m) => Pos -> String -> m c
failPos (Just (line, pos)) s = fail $ "Error in line " ++ show line ++ ", position " ++ show pos ++ ": " ++ s
failPos Nothing s = fail $ "Error: " ++ s

defaultInit :: Type Pos -> Expr Pos
defaultInit (Int pos) = ELitInt pos 0
defaultInit (Bool pos) = ELitFalse pos
defaultInit (Str pos) = EString pos ""
defaultInit (Void _) = undefined    -- this should not happen
defaultInit (Fun _ _ _) = undefined -- this should not happen

concatInstrs :: [Instr] -> String
concatInstrs = unlines
