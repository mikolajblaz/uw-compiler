module Llvm.Core where

import Control.Monad
import qualified Data.Map as Map

import AbsLatte
import ErrM

--------------------------------- Basic types -------------------------------
type Pos = Maybe (Int, Int)

-- Simple blocks
-- NOTE: it's very different than Block from Latte
type SBlock = [QInstr]

-- Block labels
type Label = Integer

-- address type
-- TODO include type information
data Addr =
    AImm Integer              -- Constant
  | AReg Integer TType        -- Temporary Registers
  | ALoc UniqueIdent          -- Local variables
  | AFun Ident                -- Functions
  | ALab Label                -- Block labels -- TODO needed?
  deriving (Show)

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
data QInstr =
    Store
  | Load
  deriving (Show)

-------------------------- Types -------------------------------------------
data TType = TInt | TStr | TBool | TVoid | TFun TType [TType]
  deriving (Show)

plainType :: Type Pos -> TType
plainType = undefined -- TODO

-------------------------- Helpers -----------------------------------------
failPos :: (Monad m) => Pos -> String -> m c
failPos (Just (line, pos)) s = fail $ "Error in line " ++ show line ++ ", position " ++ show pos ++ ": " ++ s
failPos Nothing s = fail $ "Error: " ++ s

defaultInit :: Type Pos -> Expr Pos -- TODO
defaultInit (Int pos) = ELitInt pos 0
defaultInit _ = undefined
defaultInit (Void _) = undefined  -- this should not happen

concatInstrs :: [Instr] -> Instr
concatInstrs = unlines
