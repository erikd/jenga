{-# LANGUAGE OverloadedStrings #-}
module Jenga.Config
  ( JengaConfig (..)
  , ModulesDirPath (..)
  , JengaSubmodule (..)
  , readJengaConfig
  , readJengaConfigFrom
  , mergeGitSubmodules
  , parseJengaConfig
  , renderJengaConfig
  , writeJengaConfig
  ) where

import           Control.Monad.Trans.Either (EitherT, handleIOEitherT, hoistEither)

import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.YAML (FromYAML(..), Node (..), Parser, Scalar (..), (.:), (.:?))
import qualified Data.YAML as Yaml

import           Jenga.Stack
import           Jenga.Types

import           System.FilePath ((</>))
import           System.IO.Error (isDoesNotExistError)


newtype ModulesDirPath
  = ModulesDirPath { unModulesDirPath :: FilePath }
  deriving (Eq, Show)

data JengaConfig = JengaConfig
  { jcModulesDirPath :: !ModulesDirPath
  , jcMafiaLock :: !LockFormat
  , jcDropDeps :: ![Text]
  , jcSubmodules :: ![JengaSubmodule]
  }
  deriving (Eq, Show)

data JengaSubmodule = JengaSubmodule
  { jsmUrl :: !Text
  , jsmPath :: !FilePath
  , jsmHash :: Text
  }
  deriving (Eq, Show)

instance FromYAML JengaConfig where
  parseYAML = Yaml.withMap "JengaConfig" $ \o ->
    JengaConfig
      <$> o .: "submodule-dir"
      <*> ((o .: "mafia-lock") >>= toLockFormat)
      <*> ((o .: "drop-deps") >>= parseDropDeps)
      <*> (maybe (pure []) parseSubmodules =<< (o .:? "submodules"))

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
      , "submodules" .= jcSubmodules cfg
      ]

instance ToJSON JengaSubmodule where
  toJSON jsm =
    Aeson.object
      [ "url" .= jsmUrl jsm
      , "path" .= Text.pack (jsmPath jsm)
      , "hash" .= jsmHash jsm
      ]

parseDropDeps :: Node -> Parser [Text]
parseDropDeps =
  Yaml.withSeq "parseDropDeps" $ \a ->
    mapM parseDropDep a
  where
    parseDropDep :: Node -> Parser Text
    parseDropDep (Scalar (SStr s)) = pure s
    parseDropDep invalid           = Yaml.typeMismatch "parseDropDep" invalid

parseSubmodules :: Node -> Parser [JengaSubmodule]
parseSubmodules =
  Yaml.withSeq "parseSubmodules" $ \a ->
    mapM parseSubMod a
  where
    parseSubMod =
     Yaml.withMap "JengaSubmodule" $ \o ->
        JengaSubmodule
          <$> o .: "url"
          <*> fmap Text.unpack (o .: "path")
          <*> o .: "hash"

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
readJengaConfig =
  readJengaConfigFrom configFilePath

-- Mainly for debugging and testing.
readJengaConfigFrom :: FilePath -> EitherT JengaError IO JengaConfig
readJengaConfigFrom path = do
  bs <- handleIOEitherT handler $ BS.readFile path
  hoistEither $ parseJengaConfig bs
  where
    handler err
      | isDoesNotExistError err = JengaConfigMissing
      | otherwise = JengaConfigError $ Text.pack (show err)


writeJengaConfig :: JengaConfig -> EitherT JengaError IO ()
writeJengaConfig cfg =
  handleIOEitherT handler $ BS.writeFile configFilePath (renderJengaConfig cfg)
  where
    handler =
      JengaIOError "writeJengaConfig" configFilePath


configFilePath :: FilePath
configFilePath = ".jenga"


mergeGitSubmodules :: JengaConfig -> [StackGitRepo] -> (JengaConfig, [JengaSubmodule])
mergeGitSubmodules jc sgrs =
  (newConfig, deleteSubmodules)
  where
    newConfig :: JengaConfig
    newConfig = jc { jcSubmodules = newSubmodules }

    convert :: StackGitRepo -> JengaSubmodule
    convert sgr =
      JengaSubmodule
        (sgrUrl sgr)
        (unModulesDirPath (jcModulesDirPath jc) </> Text.unpack (sgrName sgr))
        (sgrCommit sgr)

    oldSubmodules, newSubmodules, deleteSubmodules :: [JengaSubmodule]
    oldSubmodules = jcSubmodules jc
    newSubmodules = map convert sgrs
    deleteSubmodules =
      filter (\ osm -> jsmPath osm `List.notElem` map jsmPath newSubmodules) oldSubmodules
