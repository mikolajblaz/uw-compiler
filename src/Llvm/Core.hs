module Llvm.Core where

import Control.Monad
import qualified Data.Map as Map

import AbsLatte
import ErrM

--------------------------------- Basic types -------------------------------
type Pos = Maybe (Int, Int)

-- Simple blocks
-- WARNING: it's very different than Block from Latte
type SBlock = [QInstr]

-- Block labels
type Label = Integer

-- address type
data Addr =
    Immediate Integer     -- Constant
  | Reg Integer         -- Registers
  | Loc UniqueIdent         -- Local variables
  | FunA Ident           -- Functions
  | Lab Label           -- Block labels -- TODO needed?
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
