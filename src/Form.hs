{-# Language DeriveGeneric  #-}
{-# Language DeriveAnyClass #-}

module Form where

import GHC.Generics
import Data.Aeson

data FormField = FormField
  { name :: String
  , title :: String
  , description :: String
  , ftype :: String
  , enum :: Maybe [String]
  , required :: Bool
  }
  deriving (Generic, Show)


instance ToJSON FormField where
  toJSON = genericToJSON defaultOptions
    {
      fieldLabelModifier = \s -> if s == "ftype" then "type" else s
    , omitNothingFields = True
    }

instance FromJSON FormField where
  parseJSON = genericParseJSON defaultOptions
    {
      fieldLabelModifier = \s -> if s == "type" then "ftype" else s
    , omitNothingFields = True
    }
type Form = [FormField]
