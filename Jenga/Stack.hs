{-# LANGUAGE OverloadedStrings #-}

module Jenga.Stack
  ( StackConfig (..)
  , StackExtraDep (..)
  , readStackConfig
  ) where

import           Data.Aeson (FromJSON (..), Value (..), (.:))
import           Data.Aeson.Types (typeMismatch)

import qualified Data.List as DL
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
parseStackExtraDep str = do
  -- Extra-deps are of the form 'packageMame-version' where packageName itself
  -- may have a dash in it.
  let xs = T.splitOn "-" str
  if DL.length xs >= 2
    then pure $ StackExtraDep (T.intercalate "-" $ init xs) (last xs)
    else fail $ "Can't find version number in extra-dep : " <> T.unpack str

readStackConfig :: IO (Either ParseException StackConfig)
readStackConfig = Y.decodeFileEither stackYamlFile


stackYamlFile :: FilePath
stackYamlFile = "stack.yaml"
