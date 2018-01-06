module Llvm.Compiler where

import Control.Monad
import Control.Monad.Trans.State.Lazy

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
  absProgram <- pProgram $ myLexer input
  output <- liftM output $ execStateT (processProgram absProgram) (initState emptyEnv)
  -- TODO postprocess (concatenate, etc.)
  return "OK"


processProgram :: Program Pos -> GenM ()
processProgram (Program _ topDefs) = do
  buildTopEnv topDefs
  Frontend.checkMain
  mapM_ processTopDef topDefs



processTopDef :: TopDef Pos -> GenM ()
processTopDef (FnDef pos ty ident args block) = do
  startNewFun ty
  processArgs args
  -- TODO set outer env
  processBlock Nothing block -- TODO Nothing?
  Frontend.checkReturnEnding

-- | Update environment
-- | Init variables
-- | Copy values from arguments
-- | I.e. store i32, i32 %n, i32* %loc_n
processArgs :: [Arg Pos] -> GenM ()
processArgs = undefined



processBlock :: Maybe Label -> Block Pos -> GenM ()
processBlock nextL (Block pos stmts) = do
  oldBlockEnv <- gets blockEnv
  oldOuterEnv <- gets outerEnv
  let newOuterEnv = blockToOuterEnv oldBlockEnv oldOuterEnv
  setNewEnvs emptyEnv newOuterEnv
  -- Now, process block in an empty block environment
  mapM_ (processStmt Nothing) (init stmts)
  processStmt nextL (last stmts)
  -- Set old environment back, discarding everything that was inside the block
  setNewEnvs oldBlockEnv oldOuterEnv


-- processStmt stmt currentBlock nextBlock
processStmt :: Maybe Label -> Stmt Pos -> GenM ()
processStmt nextL (BStmt _ b) = processBlock nextL b

-- process single declaration
processStmt nextL (Decl pos ty [NoInit pos2 ident]) = do
  Frontend.forbidVoid ty
  processStmt nextL (Decl pos ty [Init pos2 ident (defaultInit ty)])

processStmt nextL stmt@(Decl pos ty [Init pos2 ident expr]) = do
  Frontend.forbidVoid ty
  Frontend.expectType ty expr
  -- NOTE: environment changes inside genStmt call
  Generator.genStmt nextL stmt
  undefined  -- TODO
  -- idea:
  -- tmp_var = expr
  -- int x = tmp_var --> use assignment>

-- multiple declarations
processStmt nextL (Decl pos ty items) = do
  let singleDecls = map (\item -> Decl pos ty [item]) items
  mapM_ (processStmt Nothing) (init singleDecls)
  processStmt nextL (last singleDecls)

-- other statements
processStmt nextL stmt  = do
  Frontend.checkTypeStmt stmt
  Generator.genStmt nextL stmt
