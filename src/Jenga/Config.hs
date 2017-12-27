{-# LANGUAGE OverloadedStrings #-}
module Jenga.Config
  ( JengaConfig (..)
  , readJengaConfig
  , parseJengaConfig
  , renderJengaConfig
  , writeJengaConfig
  ) where

import           Control.Monad.Extra (mapMaybeM)

import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Yaml (Parser)
import qualified Data.Yaml as Y

import           Jenga.Types

import           System.IO.Error (isDoesNotExistError, tryIOError)


data JengaConfig = JengaConfig
  { jcModulesDirPath :: !FilePath
  , jcMafiaLock :: !LockFormat
  , jcDropDeps :: ![Text]
  }
  deriving (Eq, Show)

instance FromJSON JengaConfig where
  parseJSON (Object o) =
    JengaConfig
      <$> o .: "submodule-dir"
      <*> ((o .: "mafia-lock") >>= toLockFormat)
      <*> ((o .: "drop-deps") >>= parseDropDeps)
  parseJSON invalid =
    typeMismatch "JengaConfig" invalid

toLockFormat :: Bool -> Parser LockFormat
toLockFormat b =
  pure $ if b then MafiaLock else CabalFreeze

instance ToJSON JengaConfig where
  toJSON cfg =
    Aeson.object
      [ "submodule-dir" .= jcModulesDirPath cfg
      , "mafia-lock" .= (jcMafiaLock cfg == MafiaLock)
      , "drop-deps" .= jcDropDeps cfg
      ]

parseDropDeps :: Value -> Parser [Text]
parseDropDeps v =
  case v of
    Array a -> mapMaybeM parseMaybe $ V.toList a
    invalid -> typeMismatch "parseDropDeps" invalid
  where
    parseMaybe :: Value -> Parser (Maybe Text)
    parseMaybe (String s) = pure $ Just s
    parseMaybe _ = pure Nothing

parseJengaConfig :: ByteString -> Either JengaError JengaConfig
parseJengaConfig bs =
  case Y.decodeEither bs of
    Right cfg -> Right cfg
    Left s -> Left $ JengaConfigError (T.pack s)

renderJengaConfig :: JengaConfig -> ByteString
renderJengaConfig = Y.encode

readJengaConfig :: IO (Either JengaError JengaConfig)
readJengaConfig =
  convert <$> tryIOError (BS.readFile configFilePath)
  where
    convert x =
      case x of
        Left err -> Left $ toJE err
        Right bs -> parseJengaConfig bs

    toJE err
      | isDoesNotExistError err = JengaConfigMissing
      | otherwise = JengaConfigError $ T.pack (show err)


writeJengaConfig :: JengaConfig -> IO ()
writeJengaConfig cfg = BS.writeFile configFilePath $ renderJengaConfig cfg

configFilePath :: FilePath
configFilePath = ".jenga"
