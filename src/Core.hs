module Core where

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
