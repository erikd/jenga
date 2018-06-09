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

import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.YAML (FromYAML(..), Node (..), Parser, Scalar (..), (.:))
import qualified Data.YAML as Yaml

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

instance FromYAML JengaConfig where
  parseYAML = Yaml.withMap "JengaConfig" $ \o ->
    JengaConfig
      <$> o .: "submodule-dir"
      <*> ((o .: "mafia-lock") >>= toLockFormat)
      <*> ((o .: "drop-deps") >>= parseDropDeps)

instance FromYAML ModulesDirPath where
  parseYAML = Yaml.withStr "ModulesDirPath" (pure . ModulesDirPath . Text.unpack)

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

parseDropDeps :: Node -> Parser [Text]
parseDropDeps =
    Yaml.withSeq "parseDropDeps" $ \a ->
      mapMaybeM parseMaybe a
  where
    parseMaybe :: Node -> Parser (Maybe Text)
    parseMaybe (Scalar (SStr s)) = pure $ Just s
    parseMaybe _                 = pure Nothing

parseJengaConfig :: ByteString -> Either JengaError JengaConfig
parseJengaConfig bs =
  case Yaml.decodeStrict bs of
    Right [cfg]   -> Right cfg
    Right []      -> Left $ JengaConfigError "empty configuration"
    Right (_:_:_) -> Left $ JengaConfigError "multiple documents in configuration"
    Left s        -> Left $ JengaConfigError (Text.pack s)

renderJengaConfig :: JengaConfig -> ByteString
renderJengaConfig cfg =
  LBS.toStrict $ LBS.append (Aeson.encodePretty cfg) "\n"  -- Yaml.encode

readJengaConfig :: EitherT JengaError IO JengaConfig
readJengaConfig = do
  bs <- handleIOEitherT handler $ BS.readFile configFilePath
  hoistEither $ parseJengaConfig bs
  where
    handler err
      | isDoesNotExistError err = JengaConfigMissing
      | otherwise = JengaStackError $ Text.pack (show err)


writeJengaConfig :: JengaConfig -> EitherT JengaError IO ()
writeJengaConfig cfg =
  handleIOEitherT handler $ BS.writeFile configFilePath (renderJengaConfig cfg)
  where
    handler =
      JengaIOError "writeJengaConfig" configFilePath


configFilePath :: FilePath
configFilePath = ".jenga"
