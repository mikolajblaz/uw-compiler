module Llvm.Emitter where

import Control.Monad ( liftM2 )
import Control.Monad.Trans.State.Lazy

import AbsLatte
import ErrM

import Llvm.Core
import Llvm.State

{-|
-- helper functions
printAddr :: Addr -> String
printAddr (Immediate a) = show a
printAddr (Reg a) = "%r" ++ show a
printAddr (Loc a) = "%loc" ++ show a

-- check if in range (inclusive)
inRange :: Integer -> Integer -> Integer -> Bool
inRange a l r = a >= l && a <= r

------------------------------------------------------------
-- basic function for emitting all instructions
emit :: String -> GenM ()
emit = emitIndent 2

-- add instruction to output (to the front!)
emitIndent :: Int -> String -> GenM ()
emitIndent indent str = do
  let newIntrs = replicate indent ' ' ++ str
  St env l r o <- get
  put $ St env l r (newIntrs:o)

-- specific emitters, all of them use function emit
emitBinOp :: String -> Addr -> Addr -> Addr -> GenM ()
emitBinOp op r a1 a2 = emit $
  (printAddr r) ++ " = " ++ op ++ " i32 " ++ (printAddr a1) ++ ", " ++ (printAddr a2)

emitLoad :: Addr -> Addr -> GenM ()
emitLoad src dest = emit $ (printAddr dest) ++ " = load i32, i32* " ++ printAddr src

emitStore :: Addr -> Addr -> GenM ()
emitStore src dest = emit $ "store i32 " ++ (printAddr src) ++ ", i32* " ++ (printAddr dest)

emitAlloc :: Addr -> GenM ()
emitAlloc a = emit $ (printAddr a) ++ " = alloca i32"

emitPrintInt :: Addr -> GenM ()
emitPrintInt a = emit $ "call void @printInt(i32 " ++ printAddr a ++ ")"

emitDeclarations :: GenM ()
emitDeclarations = emitIndent 0 "declare void @printInt(i32)"

emitFunctionHeader :: GenM ()
emitFunctionHeader = emitIndent 0 "define i32 @main() {"

emitFunctionEnd :: GenM ()
emitFunctionEnd = do
  emitIndent 0 "  ret i32 0"
  emitIndent 0 "}"
|-}
