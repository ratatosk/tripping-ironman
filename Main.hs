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
import System.Exit
import System.Cmd

import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS

import System.Posix.Process

type Unpacker = FilePath -> FilePath -> IO ExitCode

zipUnpacker :: Unpacker
zipUnpacker file dir = rawSystem "unzip" [encodeString file, "-d", encodeString dir]

rarUnpacker :: Unpacker
rarUnpacker file dir = rawSystem "unrar" ["x" , encodeString file, encodeString dir]

unpack :: FilePath -> FilePath -> IO ExitCode
unpack file dir =
  let ext = maybe (error "cannot get file extension") id $ extension file
  in case ext of
    "rar" -> rarUnpacker file dir
    "zip" -> zipUnpacker file dir
    _ -> error "unknown file type"

mkTempDir :: IO FilePath
mkTempDir = do
  tmp <- fromString . ("tmp_dir_" ++) . show <$> getProcessID
  createDirectory False tmp
  return tmp

--        file       temp
process :: FilePath -> FilePath -> IO ()
process file dir = do
  putStrLn $ "Temp path: " ++ encodeString dir
  putStrLn $ "File: " ++ encodeString file
  isFile file >>= flip unless (error "No such file")
  unpack file dir >>= \ec -> unless (ec == ExitSuccess) (error "Unpack failed")

main :: IO ()
main = do
  file <- decodeString . head <$> getArgs
  bracket mkTempDir removeTree (process file)