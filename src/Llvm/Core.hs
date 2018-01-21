module Llvm.Core where

import qualified Data.Map as Map

import AbsLatte
import PrintLatte

--------------------------------- Basic types -------------------------------
type Pos = Maybe (Int, Int)

-- Simple blocks
-- NOTE: it's very different than Block from Latte
type SBlock = [Instr]

-- Block labels
type Label = Integer

-- address type
data Addr =
    AImm Integer TType        -- Constant
  | AReg Integer TType        -- Temporary Registers
  | ALoc UniqueIdent TType    -- Local variables
  | AArg UniqueIdent TType    -- Function arguments
  | AStr UniqueIdent TType    -- String constants
  | AFun Ident TType          -- Functions (+ return type)
  | ALab Label                -- Block labels
  | AArr UniqueIdent TType
  | ANul TType                -- null pointer of a given type
  -- TODO class addr

instance Show Addr where
  show = printAddr

printAddr :: Addr -> String
printAddr (AImm a _) = show a
printAddr (AReg a _) = "%r" ++ show a
printAddr (ALoc (UIdent var num) _) = "%loc." ++ var ++ "." ++ show num
printAddr (AStr (UIdent _ num) _)   = "@str." ++ show num
printAddr (AArg (UIdent var num) _) = "%arg." ++ var ++ "." ++ show num
printAddr (AFun (Ident i) _) = "@" ++ i
printAddr (ALab label) = "%L" ++ show label
printAddr (AArr (UIdent var num) _) = "%arr." ++ var ++ "." ++ show num
printAddr (ANul _) = "null"

getAddrType :: Addr -> TType
getAddrType (AImm _ ty) = ty
getAddrType (AReg _ ty) = ty
getAddrType (ALoc _ ty) = TPtr ty
getAddrType (AStr _ ty) = ty
getAddrType (AArg _ ty) = ty
getAddrType (AFun _ ty) = ty
getAddrType (ALab _) = TLab
getAddrType (AArr _ ty) = ty
getAddrType (ANul ty) = ty

  ------------------------- Identifiers --------------------------------

data UniqueIdent = UIdent String Integer
  deriving (Show)

type EnvVal = (Type Pos, UniqueIdent, Addr)
  -- | Identifiers environment
type IdentEnv = Map.Map Ident EnvVal

type ClassEnv = Map.Map Ident Class

data StringConst = SConst String Addr
  deriving (Show)

-------------------------- Instructions ----------------------------------
-- Plain text instructions
type Instr = String

-------------------------- Types -------------------------------------------
data TType =
    TInt
  | TStr
  | TBool
  | TVoid
  | TPtr TType
  | TFun TType [TType]
  | TLab
  | TStrConst Integer
  | TArr TType
  | TCls Ident
  deriving (Eq)

instance Show TType where
  show TInt = "i32"
  show TStr = "i8*"
  show TBool = "i1"
  show TVoid = "void"
  show (TPtr ty) = show ty ++ "*"
  show (TFun ty _) = show ty -- TODO ?
  show TLab = "label"
  show (TStrConst len) = "[" ++ show len ++ " x i8]"
  show (TArr ty) = "{" ++ show TInt ++ ", " ++ show (TPtr ty) ++ "}"
  show (TCls (Ident i)) = "%" ++ i

printLatte :: Type Pos -> String
printLatte (Int _) = "int"
printLatte (Str _) = "string"
printLatte (Bool _) = "boolean"
printLatte (Void _) = "void"
printLatte (Arr _ ty) = printLatte ty ++ "[]"
printLatte (Cls _ (Ident i)) = i
printLatte (Fun _ ty _) = printLatte ty

plainType :: Type Pos -> TType
plainType (Int _) = TInt
plainType (Str _) = TStr
plainType (Bool _) = TBool
plainType (Void _) = TVoid
plainType (Arr _ ty) = TPtr $ plainType ty
plainType (Cls _ ident) = TCls ident
plainType (Fun _ ty tys) = TFun (plainType ty) $ map plainType tys



-------------------------- Classes -----------------------------------------
data Class = Cl {
  ty :: TType,
  attrs :: Map.Map Ident (Integer, TType) -- position, type
}
  deriving (Show)

createClass :: Ident -> [(TType, Ident)] -> Class
createClass name attrList = Cl (TCls name) $ Map.fromList enumeratedAttrs
  where
    enumeratedAttrs = zipWith (\i (ty, ident) -> (ident, (i, ty))) [0..] attrList

arrayClassName :: TType -> Ident
arrayClassName elemTy = Ident $ "_array." ++ show elemTy

arrayClass :: TType -> Class
arrayClass elemTy = createClass (arrayClassName elemTy) [
    (TPtr elemTy, Ident "_data"),
    (TInt, Ident "length")
  ]

-------------------------- Helpers -----------------------------------------
failPos :: (Monad m) => Pos -> String -> m c
failPos pos@(Just _) s = fail $ "Error in " ++ printPos pos ++ ": " ++ s
failPos Nothing s = fail $ "Error: " ++ s

printPos :: Pos -> String
printPos (Just (line, pos)) = "line " ++ show line ++ ", position " ++ show pos
printPos Nothing = ""

defaultInit :: Type Pos -> Expr Pos
defaultInit (Int pos) = ELitInt pos 0
defaultInit (Bool pos) = ELitFalse pos
defaultInit (Str pos) = EString pos ""
defaultInit (Arr pos ty) = ENull pos (Arr pos ty)
defaultInit (Void _) = undefined    -- this should not happen
defaultInit (Fun _ _ _) = undefined -- this should not happen

concatInstrs :: [Instr] -> String
concatInstrs = unlines

printTreeOneLine :: Print a => a -> String
printTreeOneLine tree = oneLiner $ printTree tree
  where
    oneLiner str = [if ch /= '\n' then ch else ' ' | ch <- str]

isFnDef :: TopDef Pos -> Bool
isFnDef (FnDef _ _ _ _ _) = True
isFnDef _ = False
