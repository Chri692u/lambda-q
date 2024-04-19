module Main where

import qualified Runtime.Env as E
import qualified Runtime.Filesystem as FS

import Language.Syntax
import Language.Types
import Language.Parser
import Language.Eval
import System.Directory
import System.FilePath
import Data.Text
import Text.Megaparsec(errorBundlePretty)

initialize :: FilePath -> IO (Either String (State Val))
initialize p = do
  -- Initial scope
  let env = E.empty :: E.Env Val
  -- Initial path
  -- Create interpreter session
  fst <- FS.fs p
  case fst of
    FS.FST fs -> do
      setCurrentDirectory p
      putStrLn $ "Initial directory: " ++ p
      let state = State env p fs
      return $ Right state
    _ -> return $ Left "Failed to initialize filesystem"

main :: IO ()
main = do
  dir <- getCurrentDirectory
  let path = dir </> "test_system"
  st <- initialize path
  case st of
    Left err -> putStrLn err
    Right state -> do
      let code = "(\\x : unit . touch x) \"test.txt\""
      let res = parseLine (pack code)
      case res of
        Left err -> putStrLn $ errorBundlePretty err
        Right exp -> do
          putStrLn $ "Parsed: " ++ show exp
          (val, s) <- eval exp state
          print val
