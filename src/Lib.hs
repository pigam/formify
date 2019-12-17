{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lib (startServer
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
import Network.Wai.Handler.Warp
import Servant
import Servant.Multipart
import Network.HTTP.Media hiding (Accept) -- for HTML ctype definition
import qualified Data.ByteString.Lazy as LBS

type Filename = String
newtype HTMLPage = HTMLPage { unRaw :: LBS.ByteString}

-- HTML mediatype
data HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML HTMLPage where
  mimeRender _ content = unRaw content


type API = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer
  :<|> "forms" :> Raw
  :<|> "form" :> Capture "filename" Filename :> Get '[HTML] HTMLPage


api :: Proxy API
api = Proxy

-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.
server :: Server API
server = upload
  :<|> getform
  :<|> generateForm
  where
    upload multipartData = do
      liftIO $ do
        putStrLn "Inputs:"
        forM_ (inputs multipartData) $ \input ->
          putStrLn $ "  " ++ show (iName input)
                ++ " -> " ++ show (iValue input)
        forM_ (files multipartData) $ \file -> do
          let content = fdPayload file
          putStrLn $ "Content of " ++ show (fdFileName file)
          LBS.putStr content
      return 0
    getform = serveDirectoryWebApp "htmlforms"
    generateForm filename = do
      pagecontent <- liftIO (LBS.readFile $ "htmlforms/" ++ filename)
      return (HTMLPage pagecontent)

app :: Application
app = serve api server

startServer :: IO ()
startServer = run 8080 app

