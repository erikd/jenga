{-# LANGUAGE OverloadedStrings #-}
module Jenga.Config
  ( JengaConfig (..)
  , readJengaConfig
  , parseJengaConfig
  , renderJengaConfig
  , writeJengaConfig
  ) where

import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Yaml as Y

import           Jenga.Types

import           System.IO.Error (isDoesNotExistError, tryIOError)


data JengaConfig = JengaConfig
  { jcModulesDirPath :: FilePath
  , jcMafiaLock :: Bool
  }
  deriving (Eq, Show)

instance FromJSON JengaConfig where
  parseJSON (Object o) =
    JengaConfig
      <$> o .: "submodule-dir"
      <*> o .: "mafia-lock"
  parseJSON invalid =
    typeMismatch "JengaConfig" invalid


instance ToJSON JengaConfig where
  toJSON cfg =
    Aeson.object
      [ "submodule-dir" .= jcModulesDirPath cfg
      , "mafia-lock" .= jcMafiaLock cfg
      ]


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
