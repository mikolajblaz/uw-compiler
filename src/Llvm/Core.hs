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
data TType = TInt | TStr | TBool | TVoid | TFun TType [TType]

instance Show TType where
  show TInt = "i32"
  show _ = "TT"

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
