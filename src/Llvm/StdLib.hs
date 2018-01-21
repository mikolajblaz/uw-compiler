module Llvm.StdLib where

import AbsLatte

import Llvm.Core


libraryTopDefs :: [TopDef Pos]
libraryTopDefs = let n = Nothing in [
    FnDef n (Void n) (Ident "printInt")    [Arg n (Int n) (Ident "i")] (Block n []),
    FnDef n (Void n) (Ident "printString") [Arg n (Str n) (Ident "i")] (Block n []),
    FnDef n (Void n) (Ident "error")       [] (Block n []),
    FnDef n (Int n)  (Ident "readInt")     [] (Block n []),
    FnDef n (Str n)  (Ident "readString")  [] (Block n []),
    FnDef n (Str n)  (Ident "concatStrings")  [Arg n (Str n) (Ident "s1"), Arg n (Str n) (Ident "s2")] (Block n []),
    FnDef n (Bool n) (Ident "compareStrings") [Arg n (Str n) (Ident "s1"), Arg n (Str n) (Ident "s2")] (Block n [])
  ]

printLibraryDeclarations :: [Instr]
printLibraryDeclarations = [
    "declare " ++ show TVoid ++ " @printInt(" ++ show TInt ++ ")",
    "declare " ++ show TVoid ++ " @printString(" ++ show TStr ++ ")",
    "declare " ++ show TVoid ++ " @error()",
    "declare " ++ show TInt  ++ " @readInt()",
    "declare " ++ show TStr  ++ " @readString()",
    "declare " ++ show TStr  ++ " @concatStrings("  ++ show TStr ++ ", " ++ show TStr ++ ")",
    "declare " ++ show TBool ++ " @compareStrings(" ++ show TStr ++ ", " ++ show TStr ++ ")",
    ""
  ]

libraryClasses :: [TopDef Pos]
libraryClasses = let n = Nothing in []

printLibraryClasses :: [Instr]
printLibraryClasses = [
    "%_array = type {i32, i64*}"
  ]
