module Main where

import Lib
import System.Directory

main :: IO ()
main = do
  createDirectoryIfMissing True datadir
  createDirectoryIfMissing True tmpdir
  startWithLogServer
