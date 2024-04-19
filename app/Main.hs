module Main where

import qualified Runtime.Env as E
import qualified Runtime.Filesystem as FS

import Language.Syntax
import Language.Types
import Language.Eval

import System.Directory
import System.FilePath
import Data.Maybe (fromJust)

main :: IO ()
main = do
  -- Initial scope
  let env = E.empty :: E.Env Val
  -- Initial path
  dir <- getCurrentDirectory
  let path = dir </> "test_system"
  -- Create interpreter session
  fst <- FS.fs path
  case fst of
    FS.FST fs -> do
      setCurrentDirectory path
      putStrLn $ "Initial directory: " ++ path
      let state = State env path fs
      let test = Con $ CString "test.txt"
      let touch = Abs "x" unitType $ Quote $ Con $ CTOUCH $ Var "x"
      let expr = Unquote (App touch test)
      putStrLn $ "Running: " ++ show expr
      (val, state') <- eval expr state
      putStrLn $ "Result: " ++ show val
      putStrLn "Current directory after running:"
      dir <- getCurrentDirectory
      print dir
    _ -> error "Cannot start interpreter without a filesystem"