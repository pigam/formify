{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib ( startWithLogServer
           , app
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
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as LBS

type Filename = String
newtype HTMLPage = HTMLPage { unRaw :: LBS.ByteString}

-- HTML mediatype
data HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML HTMLPage where
  mimeRender _ content = unRaw content


type API = "submit" :> Capture "title" String :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] Integer
  :<|> "forms" :> Raw
  :<|> "form" :> Capture "filename" Filename :> Get '[HTML] HTMLPage


api :: Proxy API
api = Proxy

uploadForm :: String -> MultipartData Tmp -> Servant.Handler Integer
uploadForm title multipartData = do
  liftIO $ do
    putStrLn $ "Title: " ++ title
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ "  " ++ show (iName input) ++ " -> " ++ show (iValue input)
    forM_ (files multipartData) $ \file -> do
      let content = fdPayload file -- MultipartResult Tmp = FilePath = String
      putStrLn $ "Content of " ++ show (fdFileName file)
      putStrLn content
    return 0
 
server :: Server API
server = uploadForm
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

app :: Application
app = serve api server


appWithConf :: FilePath -> FilePath -> Application
appWithConf tmpdir datadir = serveWithContext api context server where
  context = (opts tmpdir) :. EmptyContext

startServer :: IO ()
startServer = run 8080 app

startWithLogServer port tmpdir datadir = do
  withStdoutLogger $ \applogger -> do
    let settings = setPort port $ setLogger applogger defaultSettings
    runSettings settings $ appWithConf tmpdir datadir
