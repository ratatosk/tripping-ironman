module Main where

import Control.Monad
import Control.Exception

import System.Environment
import System.Process
import System.Directory

import System.Posix.Process

mkTempDir :: IO FilePath
mkTempDir = do
  tmp <- (("tmp_dir_" ++) . show) `liftM` getProcessID
  createDirectory tmp
  return tmp

hello :: FilePath -> IO ()
hello f = print f

main :: IO ()
main = do
  bracket mkTempDir removeDirectoryRecursive hello