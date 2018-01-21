module Llvm.Emitter where

import Control.Monad.Trans.State.Lazy
import Data.Char (ord)
import Data.List
import qualified Data.Map as Map
import Numeric (showHex)

import AbsLatte

import Llvm.Core
import Llvm.State

-- helper functions
printLabelName :: Label -> String
printLabelName label = "L" ++ show label

printRelOp :: RelOp Pos -> Char -> String
printRelOp (LTH _) sign = sign : "lt"
printRelOp (LE _) sign  = sign : "le"
printRelOp (GTH _) sign = sign : "gt"
printRelOp (GE _) sign  = sign : "ge"
printRelOp (EQU _) _ = "eq"
printRelOp (NE _) _  = "ne"

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
emitAlloc a = let (locName, ty) = split a; TPtr elemTy = ty in
  emit $ locName ++ " = alloca " ++ show elemTy

emitLoad :: Addr -> Addr -> GenM ()
emitLoad src dest = let (destReg, destTy) = split dest; (srcReg, srcTy) = split src; in
  emit $ destReg ++ " = load " ++ show destTy ++ ", " ++ printAddrTyped src -- TODO show srcTy ++ "* " ++ srcReg

emitStore :: Addr -> Addr -> GenM ()
emitStore src dest = let (destReg, destTy) = split dest in
  emit $ "store " ++ printAddrTyped src ++ ", " ++ printAddrTyped dest -- TODO show destTy ++ "* " ++ destReg

emitBinOp :: String -> Addr -> Addr -> Addr -> GenM ()
emitBinOp op r a1 a2 = let (regName, ty) = split r in
  emit $ regName ++ " = " ++ op ++ " " ++ show ty ++ " " ++ printAddr a1 ++ ", " ++ printAddr a2

emitCmp :: Addr -> RelOp Pos -> Char -> Addr -> Addr -> GenM ()
emitCmp resAddr rel sign lAddr rAddr =
  emit $ printAddr resAddr ++ " = icmp " ++ printRelOp rel sign ++ " " ++ printAddrTyped lAddr ++ ", " ++ printAddr rAddr

emitJmp :: Addr -> GenM ()
emitJmp label = emit $ "br " ++ printAddrTyped label

emitBr :: Addr -> Addr -> Addr -> GenM ()
emitBr flag l1 l2 = emit $ "br " ++ printAddrTyped flag ++ ", " ++ printAddrTyped l1 ++ ", " ++ printAddrTyped l2

emitCall :: Addr -> Addr -> [Addr] -> GenM ()
emitCall result fAddr argAddrs = emit $
  printAddr result ++ " = call " ++ printAddrTyped fAddr ++ "(" ++ outputArgs argAddrs ++ ")"

emitVoidCall :: Addr -> [Addr] -> GenM ()
emitVoidCall fAddr argAddrs = emit $
  "call " ++ printAddrTyped fAddr ++ "(" ++ outputArgs argAddrs ++ ")"

emitPhi :: Addr -> [(Addr, Addr)] -> GenM ()
emitPhi r options = let (regName, ty) = split r in emit $
  regName ++ " = phi " ++ show ty ++ " " ++ intercalate ", " (map showOpt options)
    where
      showOpt (val, label) = show [val, label]

emitConstToString :: Addr -> Addr -> GenM ()
emitConstToString r sAddr = let (regName, rTy) = split r; (sName, sTy) = split sAddr in
  emit $ regName ++ " = bitcast " ++ show sTy ++ "* " ++ sName ++ " to " ++ show rTy

emitRet :: Addr -> GenM ()
emitRet a = emit $ "ret " ++ printAddrTyped a

emitVRet :: GenM()
emitVRet = emit "ret void"

emitComment :: String -> GenM ()
emitComment comm = emit $ "  ; " ++ comm

emitCommentStmt :: (Stmt Pos) -> GenM ()
emitCommentStmt stmt = emitComment $ printTreeOneLine stmt

emitEmptyLine :: GenM ()
emitEmptyLine = emit $ ""

-- Arrays

emitArrAlloc :: Addr -> Addr -> GenM ()
emitArrAlloc res sizeAddr = let (locName, ty) = split res; TPtr elemTy = ty in
  emit $ locName ++ " = alloca " ++ show elemTy ++ ", " ++ printAddrTyped sizeAddr

emitArrLoad :: Addr -> Addr -> GenM ()
emitArrLoad = emitLoad

emitGetElement :: TType -> Addr -> Addr -> [Addr] -> GenM ()
emitGetElement baseTy res ptrAddr elemAddrs = emit $
  printAddr res ++ " = getelementptr inbounds " ++ show baseTy ++ ", " ++
    printAddrTyped ptrAddr ++ ", " ++ intercalate ", " (map printAddrTyped elemAddrs)

------------------------- Output (no state) ----------------------------------
-- Types
outputTypeDef :: TType -> [TType] -> Instr
outputTypeDef clsTy tys = show clsTy ++ " = type {" ++ intercalate ", " (map show tys) ++ "}"

outputFunction :: TType -> Ident -> [Addr] -> [Instr] -> [Instr]
outputFunction ty (Ident i) args body = header : (body ++ ["}", ""])
  where
    header = "define " ++ show ty ++ " @" ++ i ++ "(" ++ outputArgs args ++ ") {"

outputArgs :: [Addr] -> String
outputArgs args = intercalate ", " (map printAddrTyped args)

outputStringConstants :: [StringConst] -> [Instr]
outputStringConstants sConsts = reverse $ "" : (map showSConst sConsts)

showSConst :: StringConst -> Instr
showSConst (SConst str addr) = let (strName, ty) = split addr in
  strName ++ " = private constant " ++ show ty ++ " c\"" ++ showHexString str ++ "\\00\"" ++
    "  ; original string: " ++ str
      where
        showHexString str = concat $ map showHexChar str
        showHexChar char = '\\' : (showHex (ord char) "")

outputBlock :: Map.Map Label SBlock -> Label -> [Instr]
outputBlock blocks label = let instrs = Map.lookup label blocks in
  case instrs of
    Nothing -> []
    Just instrs -> (printLabelName label ++ ":") : instrs
