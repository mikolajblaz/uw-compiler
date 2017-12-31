module Llvm.Core where

import Control.Monad

type Pos = Maybe (Int, Int)

failPos :: (Monad m) => Pos -> String -> m c
failPos (Just (line, pos)) s = fail $ "Error in line " ++ show line ++ ", position " ++ show pos ++ ": " ++ s

-- Instructions
type Instruction = String

concatInstructions :: [Instruction] -> Instruction
concatInstructions = unlines

-- address type
data Addr =
  Immediate Integer     -- Constant
  | Reg Integer         -- Registers
  | Loc Integer         -- Local variables
  deriving (Show)
