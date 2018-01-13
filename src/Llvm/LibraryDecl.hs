module Llvm.LibraryDecl where

import AbsLatte

import Llvm.Core


libraryTopDefs :: [TopDef Pos]
libraryTopDefs = let n = Nothing in [
    FnDef n (Void n) (Ident "printInt")    [Arg n (Int n) (Ident "i")] (Block n []),
    FnDef n (Void n) (Ident "printString") [Arg n (Str n) (Ident "i")] (Block n []),
    FnDef n (Void n) (Ident "error")       [] (Block n []),
    FnDef n (Int n)  (Ident "readInt")     [] (Block n []),
    FnDef n (Str n)  (Ident "readString")  [] (Block n [])
  ]

printLibraryDeclarations :: [Instr]
printLibraryDeclarations = [
    "declare " ++ show TVoid ++ " @printInt(" ++ show TInt ++ ")",
    "declare " ++ show TVoid ++ " @printString(" ++ show TStr ++ ")",
    "declare " ++ show TVoid ++ " @error()",
    "declare " ++ show TInt ++ " @readInt()",
    "declare " ++ show TStr ++ " @readString()",
    ""
  ]
