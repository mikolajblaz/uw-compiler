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
  Frontend.buildTopEnv topDefs
  Frontend.checkMain
  mapM_ processTopDef topDefs



processTopDef :: TopDef Pos -> GenM ()
processTopDef (FnDef pos ty ident args block) = do
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



processBlock :: Block Pos -> SBlockLabel -> Maybe SBlockLabel -> GenM ()
processBlock (Block pos stmts) sb nextSb = do
  oldBlockEnv <- gets blockEnv
  oldOuterEnv <- gets outerEnv
  let newOuterEnv = blockToOuterEnv oldBlockEnv oldOuterEnv
  setNewEnvs emptyEnv newOuterEnv
  -- Now, process block in an empty block environment
  mapM_ processStmt stmts
  -- Set old environment back, discarding everything that was in the block
  setNewEnvs oldBlockEnv oldOuterEnv


-- processStmt stmt currentBlock nextBlock
processStmt :: Stmt Pos -> SBlockLabel -> Maybe SBlockLabel -> GenM ()
processStmt (BStmt _ b) sb nextSb = processBlock b

-- process single declaration
processStmt (Decl pos ty [NoInit pos2 ident]) =
    processStmt (Decl pos ty [Init pos2 ident (defaultInit ty)])

processStmt (Decl pos ty [Init pos2 ident exp]) = do
  Frontend.expectType ty exp
  undefined
  -- check if not void
  -- idea:
  -- tmp_var = expr
  -- int x = tmp_var --> use assignment>

-- multiple declarations
processStmt (Decl pos ty items) = do
  let singleDecls = map (\item -> Decl pos ty [item]) items
  mapM_ processStmt singleDecls

-- other statements
processStmt stmt  = do
  Frontend.checkTypeStmt stmt
  Generator.genStmt stmt
