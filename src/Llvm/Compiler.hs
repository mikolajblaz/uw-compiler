module Llvm.Compiler where

import Control.Monad

import AbsLatte
import LexLatte
import ParLatte
import ErrM

import Llvm.Core
import Llvm.State
import qualified Llvm.Generator as Generator
import qualified Llvm.Frontend as Frontend


runCompiler :: String -> Err String
runCompiler input = do
  abstractSyntax <- pProgram $ myLexer input
  processProgram abstractSyntax

  -- backendOut <- fromErrToIO $ Generator.runGenerator abstractSyntax
  -- return $ concatInstructions backendOut


processProgram :: Program Pos -> Err String -- TODO type
processProgram program = do
  topEnv <- Frontend.buildTopEnv program
  return $ "OK"


processFun :: TopDef Pos -> GenM ()
processFun (FnDef pos ty ident args block) = do
  startNewFun ty
  processArgs args
  processBlock block
  Frontend.checkReturnEnding

-- | Update environment
-- | Init variables
-- | Copy values from arguments
-- | I.e. store i32, i32 %n, i32* %loc_n
processArgs :: [Arg Pos] -> GenM ()
processArgs = undefined



processBlock :: Block Pos -> GenM ()
processBlock (Block pos stmts) = do
  mapM_ processStmt stmts


processStmt :: Stmt Pos -> GenM ()
processStmt (BStmt _ b) = processBlock b

-- process single declaration
processStmt (Decl pos ty [NoInit pos2 ident]) =
    processStmt (Decl pos ty [Init pos2 ident (defaultInit ty)])

processStmt (Decl pos ty [Init pos2 ident exp]) = do
  Frontend.expectType ty exp
  undefined
  -- idea:
  -- tmp_var = expr
  -- int x = tmp_var -- wykorzystać assignment

-- multiple declarations
processStmt (Decl pos ty items) = do
  let singleDecls = map (\item -> Decl pos ty [item]) items
  mapM_ processStmt singleDecls

-- other statements
processStmt stmt  = do
  Frontend.checkTypeStmt stmt
  Generator.genStmt stmt
