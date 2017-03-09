{-# LANGUAGE OverloadedStrings #-}

module Jenga.Stack
  ( StackConfig (..)
  , readStackConfig
  ) where

import           Data.Aeson (FromJSON (..), Value (..), (.:))
import           Data.Aeson.Types (typeMismatch)

import           Data.Text (Text)
import           Data.Yaml (ParseException)
import qualified Data.Yaml as Y


data StackConfig = StackConfig
  { stackResolver :: Text
  , stackExtraDeps :: [Text]
  }
  deriving (Eq, Show)

instance FromJSON StackConfig where
  parseJSON (Object v) = StackConfig
        <$> v .: "resolver"
        <*> v .: "extra-deps"

  parseJSON invalid = typeMismatch "StackConfig" invalid


readStackConfig :: IO (Either ParseException StackConfig)
readStackConfig = Y.decodeFileEither stackYamlFile


stackYamlFile :: FilePath
stackYamlFile = "stack.yaml"
