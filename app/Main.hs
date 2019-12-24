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
  let port = 8080
  putStrLn $ "server started on port " ++ (show port)

  createDirectoryIfMissing True datadir
  startWithLogServer port tmpdir datadir emailField
