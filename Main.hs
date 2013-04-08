{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (FilePath)

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Conditional (ifM)

import Data.String (fromString)

import Data.Trees.MTree
import Data.Bitraversable

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment
import System.Exit
import System.Cmd

import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS

import ID3.Type.Tag
import ID3.ReadTag

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

-- readTag sucks because of weird dependencies
tryReadTag :: FilePath -> IO (Maybe ID3Tag)
tryReadTag f = withFile f ReadMode hReadTag


data Track = Track { artist :: Maybe T.Text
                   , album :: Maybe T.Text
                   , title :: Maybe T.Text
                   } deriving (Show)

readTree :: FilePath -> IO (Tree FilePath FilePath)
readTree p = liftM (Node p) $ listDirectory p >>= mapM readChild
  where
    readChild c = ifM (isDirectory c) (readTree c) (return $ Leaf c c)    

--        file       temp
process :: FilePath -> FilePath -> IO ()
process file dir = do
  putStrLn $ "Temp path: " ++ encodeString dir
  putStrLn $ "File: " ++ encodeString file
  isFile file >>= flip unless (error "No such file")
  unpack file dir >>= \ec -> unless (ec == ExitSuccess) (error "Unpack failed")
  readTree dir >>= bitraverse return tryReadTag >>= print

main :: IO ()
main = do
  file <- decodeString . head <$> getArgs
  bracket mkTempDir removeTree (process file)