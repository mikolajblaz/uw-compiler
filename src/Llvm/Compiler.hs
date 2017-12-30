module Main where

import Control.Exception
import Control.Monad
import System.Environment ( getArgs )
import System.Exit (exitFailure)
import System.FilePath.Posix
import qualified System.Directory as SD
import System.IO
import System.Process

import AbsInstant
import ErrM

import Core
import qualified Llvm.Generator

main :: IO ()
main = do
  args <- getArgs
  catch (processArgs args) handleError


handleError :: IOException -> IO a
handleError e = do
  let err = show e
  hPutStrLn stderr $ "ERROR: " ++ err
  exitFailure


processArgs :: [String] -> IO ()
processArgs args = do
  unless (length args == 1) $ throw $ userError "Run compiler with exactly 1 argument"

  let fileName = head args
  exists <- SD.doesFileExist fileName
  unless exists $ throw $ userError $ "File " ++ fileName ++ " doesn't exist"

  let extension = takeExtension fileName
  unless (extension == ".ins") $ throw $ userError "Wrong file extension (must be .ins)"

  input <- readFile fileName;
  output <- runCompiler input

  let llFile = replaceExtension fileName ".ll"
  let bcFile = replaceExtension fileName ".bc"
  writeFile llFile output

  -- assemble
  -- llvm-as -o in.bc in.ll
  asOutput <- readProcess "llvm-as" ["-o", bcFile, llFile] ""
  putStr asOutput

  -- link with runtime (overwrite bcFile)
  -- llvm-link -o in.bc in.bc runtime.bc
  linkOutput <- readProcess "llvm-link" ["-o", bcFile, bcFile, "lib/runtime.bc"] ""
  putStr linkOutput


runCompiler :: String -> IO String
runCompiler input = case Llvm.Generator.runGenerator input of
    Ok s -> return $ concatInstructions s
    Bad s -> throw $ userError s
