{-# LANGUAGE OverloadedStrings #-}
module Jenga.Config
  ( JengaConfig (..)
  , ModulesDirPath (..)
  , readJengaConfig
  , parseJengaConfig
  , renderJengaConfig
  , writeJengaConfig
  ) where

import           Control.Monad.Extra (mapMaybeM)
import           Control.Monad.Trans.Either (EitherT, handleIOEitherT, hoistEither)

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

import           System.IO.Error (isDoesNotExistError)


newtype ModulesDirPath
  = ModulesDirPath { unModulesDirPath :: FilePath }
  deriving (Eq, Show)

data JengaConfig = JengaConfig
  { jcModulesDirPath :: !ModulesDirPath
  , jcMafiaLock :: !LockFormat
  , jcDropDeps :: ![Text]
  }
  deriving (Eq, Show)

instance FromJSON JengaConfig where
  parseJSON (Object o) =
    JengaConfig
      <$> (ModulesDirPath <$> o .: "submodule-dir")
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
      [ "submodule-dir" .= unModulesDirPath (jcModulesDirPath cfg)
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

readJengaConfig :: EitherT JengaError IO JengaConfig
readJengaConfig = do
  bs <- handleIOEitherT handler $ BS.readFile configFilePath
  hoistEither $ parseJengaConfig bs
  where
    handler err
      | isDoesNotExistError err = JengaConfigMissing
      | otherwise = JengaStackError $ T.pack (show err)


writeJengaConfig :: JengaConfig -> EitherT JengaError IO ()
writeJengaConfig cfg =
  handleIOEitherT handler $ BS.writeFile configFilePath (renderJengaConfig cfg)
  where
    handler =
      JengaIOError "writeJengaConfig" configFilePath


configFilePath :: FilePath
configFilePath = ".jenga"
