module Llvm.Emitter where

import Control.Monad.Trans.State.Lazy
import Data.List

import AbsLatte
import PrintLatte

import Llvm.Core
import Llvm.State

-- helper functions
printAddr :: Addr -> String
printAddr (AImm a _) = show a
printAddr (AReg a _) = "%r" ++ show a
printAddr (ALoc (UIdent var num) _) = "%loc" ++ show num ++ "_" ++ var
printAddr (AArg (UIdent var num) _) = "%arg" ++ show num ++ "_" ++ var
printAddr (AFun (Ident i) _) = "@" ++ i
printAddr (ALab label) = "%L" ++ show label

printLabelName :: Label -> String
printLabelName label = "L" ++ show label

printAddrTyped :: Addr -> String
printAddrTyped addr = let (addrName, addrTy) = split addr in
  show addrTy ++ " " ++ addrName

getAddrType :: Addr -> TType
getAddrType (AImm _ ty) = ty
getAddrType (AReg _ ty) = ty
getAddrType (ALoc _ ty) = ty
getAddrType (AArg _ ty) = ty
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
emitAlloc a = let (locName, ty) = split a in
  emit $ locName ++ " = alloca " ++ show ty

emitLoad :: Addr -> Addr -> GenM ()
emitLoad src dest = let (destReg, destTy) = split dest; (srcReg, srcTy) = split src;  in
  emit $ destReg ++ " = load " ++ show destTy ++ ", " ++ show srcTy ++ "* " ++ srcReg

emitStore :: Addr -> Addr -> GenM ()
emitStore src dest = let (destReg, destTy) = split dest in
  emit $ "store " ++ printAddrTyped src ++ ", " ++ show destTy ++ "* " ++ destReg

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

emitCall :: Addr -> [Addr] -> GenM ()
emitCall fAddr argAddrs = emit $ "call " ++ printAddrTyped fAddr ++ "(" ++ outputArgs argAddrs ++ ")" -- TODO

emitRet :: Addr -> GenM ()
emitRet a = emit $ "ret " ++ printAddrTyped a

emitVRet :: GenM()
emitVRet = emit "ret void"

emitComment :: String -> GenM ()
emitComment comm = emit $ "  ; " ++ comm

emitCommentStmt :: (Stmt Pos) -> GenM ()
emitCommentStmt stmt = emitComment $ oneLiner $ printTree stmt
  where
    oneLiner str = [if ch /= '\n' then ch else ' ' | ch <- str]

emitEmptyLine :: GenM ()
emitEmptyLine = emit $ ""

-- TODO remove
-- emitPrintInt :: Addr -> GenM ()
-- emitPrintInt a = emit $ "call void @printInt(i32 " ++ printAddr a ++ ")"

------------------------- Output ---------------------------------------
outputFunction :: TType -> Ident -> [Addr] -> [Instr] -> [Instr]
outputFunction ty (Ident i) args body = header : (body ++ ["}", ""])
  where
    header = "define " ++ show ty ++ " @" ++ i ++ "(" ++ outputArgs args ++ ") {"

outputArgs :: [Addr] -> String
outputArgs args = intercalate ", " (map printAddrTyped args)
