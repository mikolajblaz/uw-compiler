module Llvm.Compiler where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import Data.Maybe

import AbsLatte
import LexLatte
import ParLatte
import ErrM

import Llvm.Core
import Llvm.State
import qualified Llvm.Generator as Generator
import qualified Llvm.Frontend as Frontend
import qualified Llvm.Emitter as Emitter


runCompiler :: String -> Err String
runCompiler input = do
  absProgram <- pProgram $ myLexer input
  newAbsProgram <- evalStateT (Frontend.analyzeProgram absProgram) (initState emptyEnv)
  evalStateT (Generator.processProgram newAbsProgram) (initState emptyEnv)
