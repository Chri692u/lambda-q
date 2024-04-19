module Runtime.Filesystem where

import qualified Runtime.Env as E

import System.Directory
import System.FilePath
import Data.Maybe(fromJust)
import Control.Monad (liftM)

-- Paths in the filesystem
type Path = FilePath
type Paths = [Path]

-- A Forest of Filesystem Trees
type FST = E.Env FVal

-- Values in the filesystem
data FVal = FAlias Paths
          | FFile Path
          | FST FST

instance Show FVal where
  show (FAlias paths) = "Alias: " ++ unwords paths
  show (FFile path) = path
  show (FST fs) = unwords (E.keys fs)

-- Convert a Path to a FVal
fs :: Path -> IO FVal
fs p = do
  isDir <- doesDirectoryExist p
  isFile <- doesFileExist p
  if isDir
    then do
      contents <- getDirectoryContents p
      let paths = map (p </>) $ filter (`notElem` [".", ".."]) contents
      fvals <- mapM fs paths
      return $ FST $ E.fromList $ zip paths fvals
    else if isFile
      then return $ FFile p
      else return $ FAlias [p]