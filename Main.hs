{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (FilePath)

import Control.Applicative
import Control.Monad
import Control.Exception

import Data.String (fromString)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment
import System.Process

import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS

import System.Posix.Process

showPath :: FilePath -> T.Text
showPath = either id id . toText

mkTempDir :: IO FilePath
mkTempDir = do
  tmp <- fromString . ("tmp_dir_" ++) . show <$> getProcessID
  createDirectory False tmp
  return tmp

--      file       temp
hello :: FilePath -> FilePath -> IO ()
hello f t = do
  T.putStrLn $ "Temp path: " `T.append` showPath t
  T.putStrLn $ "File: " `T.append` showPath f

main :: IO ()
main = do
  file <- head <$> getArgs
  bracket mkTempDir removeTree (hello $ fromString file)