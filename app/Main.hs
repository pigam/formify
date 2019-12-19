module Main where

import Lib
import System.Directory

datadir :: FilePath
datadir = "/tmp/formify"

tmpdir :: FilePath
tmpdir = "/tmp/formify_tmp"

main :: IO ()
main = do
  createDirectoryIfMissing True datadir
  startWithLogServer tmpdir datadir
