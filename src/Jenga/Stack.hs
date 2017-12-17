{-# LANGUAGE OverloadedStrings #-}

module Jenga.Stack
  ( StackConfig (..)
  , StackExtraDep (..)
  , StackFilePath (..)
  , StackGitRepo (..)
  , readStackConfig
  ) where

import           Control.Monad.Extra (mapMaybeM)

import           Data.Aeson (FromJSON (..), Value (..), (.:))
import           Data.Aeson.Types (typeMismatch)

import qualified Data.ByteString.Char8 as BS

import qualified Data.List as DL
import           Data.Monoid ((<>))

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Yaml (ParseException, Parser)
import qualified Data.Yaml as Y

import           Jenga.Types


newtype StackFilePath = StackFilePath FilePath

data StackConfig = StackConfig
  { stackResolver :: !Text
  , stackExtraDeps :: ![StackExtraDep]
  , stackGitLocations :: ![StackGitRepo]
  }
  deriving (Eq, Show)

instance FromJSON StackConfig where
  parseJSON (Object o) = StackConfig
        <$> o .: "resolver"
        <*> o .: "extra-deps"
        <*> ((o .: "packages") >>= parseStackGitRepos)

  parseJSON invalid = typeMismatch "StackConfig" invalid

parseStackGitRepos :: Value -> Parser [StackGitRepo]
parseStackGitRepos v =
  case v of
    Array a -> mapMaybeM parseMaybe $ V.toList a
    invalid -> typeMismatch "StackConfig" invalid
  where
    parseMaybe :: Value -> Parser (Maybe StackGitRepo)
    parseMaybe (Object o) =
      fmap Just $
        (o .: "location") >>= \p ->
          StackGitRepo
            <$> p .: "git"
            <*> p .: "commit"
    parseMaybe _ = pure Nothing


data StackExtraDep = StackExtraDep
  { sedName :: !Text
  , sedVersion :: !Version
  } deriving (Eq, Show)

data StackGitRepo = StackGitRepo
  { sgrUrl :: !Text
  , sgrCommit :: !Text
  } deriving (Eq, Show)

instance FromJSON StackGitRepo where
  parseJSON (Object o) = StackGitRepo
        <$> o .: "git"
        <*> o .: "commit"
  parseJSON invalid = typeMismatch "StackGitRepo" invalid

instance FromJSON StackExtraDep where
  parseJSON (String s) = parseStackExtraDep s
  parseJSON invalid = typeMismatch "StackExtraDep" invalid

parseStackExtraDep :: Text -> Parser StackExtraDep
parseStackExtraDep str = do
  -- Extra-deps are of the form 'packageName-version' where packageName itself
  -- may have a dash in it.
  let xs = T.splitOn "-" str
  if DL.length xs >= 2
    then pure $ StackExtraDep (T.intercalate "-" $ init xs) (readVersion $ last xs)
    else fail $ "Can't find version number in extra-dep : " <> T.unpack str

readStackConfig :: StackFilePath -> IO (Either ParseException StackConfig)
readStackConfig (StackFilePath stackYamlFile) =
  Y.decodeEither' <$> BS.readFile stackYamlFile
