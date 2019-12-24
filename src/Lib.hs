{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib ( startWithLogServer
           , app
           , FormField
           ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Text.Encoding (encodeUtf8)
import Network.Socket (withSocketsDo)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Network.Wai.Handler.Warp (run, runSettings, setLogger, defaultSettings,setPort)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Parse (defaultParseRequestBodyOptions)
import Servant
import Servant.Multipart
import Network.HTTP.Media hiding (Accept) -- for HTML ctype definition
import System.Directory
import qualified Data.ByteString.Lazy as LBS
import Data.UUID
import Data.UUID.V4
import System.FilePath
import Data.Text as Text
import Data.MIME.Types as MIME
type Filename = String
type FormField = Text
newtype HTMLPage = HTMLPage { unRaw :: LBS.ByteString}

-- HTML mediatype
data HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML HTMLPage where
  mimeRender _ content = unRaw content


type API = "submit" :> Capture "title" String :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] String
  :<|> "forms" :> Raw
  :<|> "form" :> Capture "filename" Filename :> Get '[HTML] HTMLPage


api :: Proxy API
api = Proxy

-- must return resByUUIDDir
createStorageDirectories :: FilePath -> Text -> IO FilePath
createStorageDirectories storageDir email = do
  let resByEmail = storageDir </> "results" </> (Text.unpack email)
        -- test
  exists <- catch (do
    link <- pathIsSymbolicLink resByEmail
    target <- getSymbolicLinkTarget resByEmail
    exists <- doesDirectoryExist target
    if exists
      then
        return $ Just target
      else
        return Nothing)
            (\e -> do
                let err = show (e :: IOException)
                putStrLn err
                return Nothing)
  case exists of
    Just target -> do return target
    _ -> do
      uuid <- nextRandom
      let resByUUIDDir = storageDir </> "results_by_uuid" </> (toString uuid)
      createDirectoryIfMissing True resByUUIDDir
      createDirectoryIfMissing False $ storageDir </> "results"
      createDirectoryLink resByUUIDDir resByEmail
      return resByUUIDDir


fileTransform :: [FileData t] -> [(FileData t, Integer, Text)]
fileTransform  = Prelude.foldr ff [] where
  ff f [] = [(f, 0, fdInputName f)]
  ff f ((g, n, t): old) 
        | fdInputName f == t, n == 0  = ( f, 2, fdInputName f) : (g,1,t) : old
        | fdInputName f == t = ( f, n+1, fdInputName f) : (g,n,t) : old
        | otherwise = (f, 0, fdInputName f) : (g, n, t) : old

format :: Text -> Integer -> String
format t 0 = unpack $ Text.drop 9 t
format t n = (unpack $ Text.drop 9 t) ++ "_" ++ (show n)

uploadForm :: FilePath -> FormField -> String -> MultipartData Tmp -> Servant.Handler String
uploadForm datadir emailField title multipartData = 
  do
    case lookupInput emailField multipartData of
      Just email -> liftIO $ do
        let storageDir = datadir </> title
        let resByEmail = storageDir </> "results" </> (Text.unpack email)
        resByUUIDDir <- createStorageDirectories storageDir email
        putStrLn $ "Title: " ++ title
        putStrLn "Inputs:"
        forM_ (inputs multipartData) $ \input ->
          putStrLn $ "  " ++ show (iName input) ++ " -> " ++ show (iValue input)
        putStrLn "files transformed : "
        forM_ (fileTransform $ files multipartData) $ \(fd, n, inputname) -> do
          let extension = case MIME.guessExtension MIME.defaultmtd True (unpack $ fdFileCType fd) of
                Just ext -> ext
                Nothing -> ""
          let targetName = resByUUIDDir </> (format inputname n) -<.> extension
 -- putStrLn $ "fdFileName : " ++ (show $ fdFileName fd)
          -- putStrLn $ "fdPayload : " ++ (show $ fdPayload fd)
          -- putStrLn $ "fdInputName : " ++ (show inputname) ++ "_" ++ (show n)
          -- putStrLn $ "mime type : " ++ (show $ fdFileCType fd)
          -- putStrLn $ "target : " ++ targetName
          copyFile (fdPayload fd) targetName

        -- forM_ (files multipartData) $ \file -> do
        --   let content = fdPayload file -- MultipartResult Tmp = FilePath = String
        --   putStrLn $ "Content of " ++ show (fdFileName file) ++ " : "
        --   putStr content
        --   putStrLn ""
        --   putStrLn $ "formField : " ++ show (fdInputName file)
        return "ok"
      Nothing -> throwError (err400 {
          errBody = LBS.fromStrict . encodeUtf8 . pack   $ "missing email value in " ++ (unpack emailField) ++ " field"
        })

 
server :: FilePath -> FormField -> Server API
server datadir emailField = uploadForm datadir emailField
  :<|> getform
  :<|> generateForm
  where
    getform = serveDirectoryWebApp "htmlforms"
    generateForm filename = do
      pagecontent <- liftIO (LBS.readFile $ "htmlforms/" ++ filename)
      return (HTMLPage pagecontent)

opts :: FilePath -> MultipartOptions Tmp
opts tmpdir = MultipartOptions {
    generalOptions = defaultParseRequestBodyOptions
  , backendOptions = TmpBackendOptions
    {
      getTmpDir = do
        createDirectoryIfMissing False tmpdir
        return tmpdir
    , filenamePat = "formify-file.buf"
    }
  }

app :: FilePath -> FormField -> Application
app datadir emailField = serve api $ server datadir emailField


appWithConf :: FilePath -> FilePath -> FormField -> Application
appWithConf tmpdir datadir emailField = serveWithContext api context (server datadir emailField) where
  context = (opts tmpdir) :. EmptyContext


startWithLogServer port tmpdir datadir emailField = do
  createDirectoryIfMissing True tmpdir
  createDirectoryIfMissing True datadir
  withStdoutLogger $ \applogger -> do
    let settings = setPort port $ setLogger applogger defaultSettings
    runSettings settings $ appWithConf tmpdir datadir emailField
