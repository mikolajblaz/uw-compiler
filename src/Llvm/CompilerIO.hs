module Main where

import Control.Exception
import Control.Monad
import System.Environment ( getArgs )
import System.Exit (exitFailure)
import System.FilePath.Posix
import qualified System.Directory as SD
import System.IO
import System.Process

import AbsLatte
import LexLatte
import ParLatte
import ErrM

import qualified Llvm.Compiler as Compiler

main :: IO ()
main = do
  args <- getArgs
  catch (processArgs args) handleError


handleError :: IOException -> IO a
handleError err = do
  hPutStrLn stderr $ "ERROR\n" ++ show err
  exitFailure


processArgs :: [String] -> IO ()
processArgs args = do
  unless (length args == 1) $ throw $ userError "Run compiler with exactly 1 argument"

  let fileName = head args
  exists <- SD.doesFileExist fileName
  unless exists $ throw $ userError $ "File " ++ fileName ++ " doesn't exist"

  let extension = takeExtension fileName
  unless (extension == ".lat") $ throw $ userError "Wrong file extension (must be .lat)"

  input <- readFile fileName;
  output <- fromErrToIO $ Compiler.runCompiler input

  let llFile = replaceExtension fileName ".ll"
  let bcFile = replaceExtension fileName ".bc"
  writeFile llFile output

  hPutStrLn stderr $ "OK\n"

  -- assemble
  -- llvm-as -o in.bc in.ll
  asOutput <- readProcess "llvm-as" ["-o", bcFile, llFile] ""
  putStr asOutput

  -- link with runtime (overwrite bcFile)
  -- llvm-link -o in.bc in.bc runtime.bc
  linkOutput <- readProcess "llvm-link" ["-o", bcFile, bcFile, "lib/runtime.bc"] ""
  putStr linkOutput




fromErrToIO :: Err a -> IO a
fromErrToIO out = case out of
  Ok s -> return s
  Bad s -> throw $ userError s
