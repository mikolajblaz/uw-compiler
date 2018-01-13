module Llvm.Emitter where

import Control.Monad.Trans.State.Lazy

import AbsLatte
import ErrM

import Llvm.Core
import Llvm.State

-- helper functions
printAddr :: Addr -> String
printAddr (AImm a _) = show a
printAddr (AReg a _) = "%r" ++ show a
printAddr (ALoc (UIdent var num) _) = "%loc" ++ show num ++ "_" ++ show var
printAddr (AFun ident ty) = undefined
printAddr (ALab label) = "%L" ++ show label

printAddrTyped :: Addr -> String
printAddrTyped addr = let (addrName, addrTy) = split addr in
  show addrTy ++ " " ++ addrName

getAddrType :: Addr -> TType
getAddrType (AImm _ ty) = ty
getAddrType (AReg _ ty) = ty
getAddrType (ALoc _ ty) = ty
getAddrType (AFun _ ty) = ty
getAddrType (ALab _) = TLab

split :: Addr -> (String, TType)
split a = (printAddr a, getAddrType a)

printRelOp :: RelOp Pos -> String
printRelOp (LTH _) = "slt"
printRelOp (LE _) = "sle"
printRelOp (GTH _) = "sgt"
printRelOp (GE _) = "sge"
printRelOp (EQU _) = "eq"
printRelOp (NE _) = "ne"

------------------------------------------------------------
-- basic function for emitting all instructions
emit :: Instr -> GenM ()
emit = emitIndent 2

-- add instruction to output (to the front!)
emitIndent :: Int -> Instr -> GenM ()
emitIndent indent instr = do
  let newIntrs = replicate indent ' ' ++ instr
  modify $ addInstr newIntrs

--------- specific emitters, all of them use function emit ------------------

emitAlloc :: Addr -> GenM ()
emitAlloc a = let (regName, ty) = split a in
  emit $ regName ++ " = alloca " ++ show ty

emitLoad :: Addr -> Addr -> GenM ()
emitLoad src dest = let (destReg, destTy) = split dest in
  emit $ destReg ++ " = load " ++ show destTy ++ ", " ++ printAddrTyped src

emitStore :: Addr -> Addr -> GenM ()
emitStore src dest = emit $ "store " ++ printAddrTyped src ++ ", " ++ printAddrTyped dest

emitBinOp :: String -> Addr -> Addr -> Addr -> GenM ()
emitBinOp op r a1 a2 = let (regName, ty) = split r in
  emit $ regName ++ " = " ++ op ++ " " ++ show ty ++ " " ++ printAddr a1 ++ ", " ++ printAddr a2

emitCmp :: Addr -> RelOp Pos -> Addr -> Addr -> GenM ()
emitCmp resAddr rel lAddr rAddr =
  emit $ printAddr resAddr ++ " = icmp " ++ printRelOp rel ++ " " ++ printAddrTyped lAddr ++ ", " ++ printAddr rAddr

emitJmp :: Addr -> GenM ()
emitJmp label = emit $ "br " ++ printAddrTyped label

emitBr :: Addr -> Addr -> Addr -> GenM ()
emitBr flag l1 l2 = emit $ "br " ++ printAddrTyped flag ++ ", " ++ printAddrTyped l1 ++ ", " ++ printAddrTyped l2

emitCall :: Addr -> GenM ()
emitCall fAddr = emit $ "call" -- TODO

emitRet :: Addr -> GenM ()
emitRet a = emit $ "ret " ++ printAddrTyped a

emitVRet :: GenM()
emitVRet = emit "ret void"

-- TODO remove
-- emitPrintInt :: Addr -> GenM ()
-- emitPrintInt a = emit $ "call void @printInt(i32 " ++ printAddr a ++ ")"

------------------------- Output ---------------------------------------

outputInstr :: [Instr] -> GenM [Instr]
-- outputInstr instrs = modify (addOutput instrs)
outputInstr = return

outputFunctionFrame :: TType -> Ident -> (String -> [Instr] -> [Instr])
outputFunctionFrame ty (Ident i) args body = header : (body ++ ["}", ""])
  where
    header = "define " ++ show ty ++ " @" ++ i ++ "(" ++ args ++ ") {"
