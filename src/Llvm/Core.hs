module Llvm.Core where

import Control.Monad

import AbsLatte
import ErrM

--------------------------------- Basic types -------------------------------
type Pos = Maybe (Int, Int)

type Instr = String

-- address type
data Addr =
    Immediate Integer     -- Constant
  | Reg Integer         -- Registers
  | Loc Integer         -- Local variables
  deriving (Show)


-- Intermediate instructions (quadruple-code)
data QInstr =
    Store
  | Load
  deriving (Show)


-- Simple blocks
-- WARNING: it's very different than Block from Latte
type SBlock = [QInstr]
type SBlockLabel = Integer

-------------------------- Helpers -----------------------------------------
failPos :: (Monad m) => Pos -> String -> m c
failPos (Just (line, pos)) s = fail $ "Error in line " ++ show line ++ ", position " ++ show pos ++ ": " ++ s

defaultInit :: Type Pos -> Expr Pos -- TODO
defaultInit (Int pos) = ELitInt pos 0
defaultInit _ = undefined
defaultInit (Void pos) = undefined  -- this should not happen

concatInstrs :: [Instr] -> Instr
concatInstrs = unlines
