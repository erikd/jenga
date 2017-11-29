{-# LANGUAGE OverloadedStrings #-}

module Jenga.Stack
  ( StackConfig (..)
  , StackExtraDep (..)
  , StackFilePath (..)
  , readStackConfig
  ) where

import           Data.Aeson (FromJSON (..), Value (..), (.:))
import           Data.Aeson.Types (typeMismatch)

import qualified Data.ByteString.Char8 as BS

import qualified Data.List as DL
import           Data.Monoid ((<>))

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Yaml (ParseException, Parser)
import qualified Data.Yaml as Y


newtype StackFilePath = StackFilePath FilePath

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

readStackConfig :: StackFilePath -> IO (Either ParseException StackConfig)
readStackConfig (StackFilePath stackYamlFile) =
  Y.decodeEither' . cleanLines <$> BS.readFile stackYamlFile
  where
    cleanLines =
      BS.unlines . map (BS.takeWhile (/= '#')) . BS.lines
