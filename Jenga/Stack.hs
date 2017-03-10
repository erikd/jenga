{-# LANGUAGE OverloadedStrings #-}

module Jenga.Stack
  ( StackConfig (..)
  , StackExtraDep (..)
  , readStackConfig
  ) where

import           Data.Aeson (FromJSON (..), Value (..), (.:))
import           Data.Aeson.Types (typeMismatch)

import           Data.Monoid ((<>))

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Yaml (ParseException, Parser)
import qualified Data.Yaml as Y


data StackConfig = StackConfig
  { stackResolver :: !Text
  , stackExtraDeps :: ![StackExtraDep]
  }
  deriving (Eq, Show)

instance FromJSON StackConfig where
  parseJSON (Object v) = StackConfig
        <$> v .: "resolver"
        <*> v .: "extra-deps"

  parseJSON invalid = typeMismatch "StackConfig" invalid


data StackExtraDep
  = StackExtraDep !Text !Text
  deriving (Eq, Show)

instance FromJSON StackExtraDep where
  parseJSON (String s) = parseStackExtraDep s
  parseJSON invalid = typeMismatch "StackExtraDep" invalid

parseStackExtraDep :: Text -> Parser StackExtraDep
parseStackExtraDep str =
  case T.splitOn "-" str of
    [] -> fail $ "Can't find version number in extra-dep : " <> T.unpack str
    [_] -> fail $ "Can't find version number in extra-dep : " <> T.unpack str
    xs -> pure $ StackExtraDep (T.intercalate "-" $ init xs) (last xs)


readStackConfig :: IO (Either ParseException StackConfig)
readStackConfig = Y.decodeFileEither stackYamlFile


stackYamlFile :: FilePath
stackYamlFile = "stack.yaml"
