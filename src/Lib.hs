{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import System.FilePath
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.UTF8 as BSU
import Data.UUID as UUID
import Data.UUID.V4
import System.FilePath
import Data.Text as Text
import Data.MIME.Types as MIME
import qualified Data.Csv as Csv (encode)
import qualified Data.ByteString.Lazy.Char8 as BL8 (putStr, putStrLn, writeFile)
import Servant.HTML.Lucid
import Lucid
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics 
type Filename = String
type FormField = Text
type Email = String

newtype HTMLPage = HTMLPage { unRaw :: LBS.ByteString}

-- HTML mediatype
data RawHTML
instance Accept RawHTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender RawHTML HTMLPage where
  mimeRender _ content = unRaw content

data Submission = Submission { uuid :: String
                             , email :: Email
                             } deriving (Eq, Show, Generic)

instance ToJSON Submission
instance ToHtml Submission where
  toHtml sub =
    dl_ $ do
      dt_ "unique token"
      dd_ (toHtml $ uuid sub)
      dt_ "email"
      dd_ (toHtml $ email sub)
  toHtmlRaw = toHtml
  
type API = "submit" :> Capture "title" String :> MultipartForm Tmp (MultipartData Tmp) :> Post '[HTML, JSON] Submission
  :<|> "forms" :> Raw
  :<|> "form" :> Capture "filename" Filename :> Get '[RawHTML] HTMLPage


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
      let resByUUIDDir = storageDir </> "results_by_uuid" </> (UUID.toString uuid)
      createDirectoryIfMissing True resByUUIDDir
      createDirectoryIfMissing False $ storageDir </> "results"
      createDirectoryIfMissing False $ storageDir </> "mail"
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


uploadForm :: FilePath -> FormField -> String -> MultipartData Tmp -> Servant.Handler Submission
uploadForm datadir emailField title multipartData = 
  case lookupInput emailField multipartData of
    Just email -> liftIO $ do
      let storageDir = datadir </> title
      let resByEmail = storageDir </> "results" </> (Text.unpack email)
      resByUUIDDir <- createStorageDirectories storageDir email
      let uuid = takeBaseName resByUUIDDir
      -- generate csv from non files inputs
      let csvheaders = fmap iName (inputs multipartData)
      let csvvals = fmap iValue (inputs multipartData)
      BL8.writeFile (resByUUIDDir </> "result.csv") $ Csv.encode [csvheaders, csvvals]
      -- copy uploaded files to resByUUIDDir
      forM_ (fileTransform $ files multipartData) $ \(fd, n, inputname) -> do
        let extension = case MIME.guessExtension MIME.defaultmtd True (unpack $ fdFileCType fd) of
              Just ext -> ext
              Nothing -> ""
        let targetName = resByUUIDDir </> (format inputname n) -<.> extension
        copyFile (fdPayload fd) targetName
      let submission = Submission uuid (Text.unpack email)
      let jsonmail = storageDir </> "mail" </> (Text.unpack email) <.> "json"
      encodeFile jsonmail submission
      return submission
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
