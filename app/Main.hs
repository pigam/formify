{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import System.Directory

datadir :: FilePath
datadir = "/tmp/formify"

tmpdir :: FilePath
tmpdir = "/tmp/formify_tmp"

emailField :: FormField
emailField = "fy-pi-email"
main :: IO ()

main = do
  createDirectoryIfMissing True datadir
  startWithLogServer 8080 tmpdir datadir emailField
