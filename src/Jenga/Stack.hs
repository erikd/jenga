{-# LANGUAGE OverloadedStrings #-}
module Jenga.Stack
  ( ConfigExtraDep (..)
  , StackLocalDir (..)
  , StackConfig
  , StackExtraDep (..)
  , StackFilePath (..)
  , StackGitRepo (..)
  , mkStackConfig
  , parseStackConfig
  , readStackConfig
  , renderStackConfig
  , stackExtraDeps
  , stackGitRepos
  , stackLocalDirs
  , stackResolver
  ) where

import           Control.Applicative (optional)
import           Control.Monad.Extra (mapMaybeM)
import           Control.Monad.Trans.Either (EitherT, handleIOEitherT, hoistEither)

import           Data.Aeson (ToJSON (..), Value (..), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty', defConfig)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as List
import           Data.Monoid ((<>))

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.YAML (FromYAML (..), Node (..), Parser, Scalar (..), (.:), (.:?))
import qualified Data.YAML as Yaml

import           Jenga.Types

import           System.IO.Error (isDoesNotExistError)

newtype StackFilePath
  = StackFilePath { unstackFilePath :: FilePath }

-- | StackConfig needs to be an opaque type because git repos (with hashes) can
-- be listed in either the 'extra-deps` field of the 'locations' field (which
-- ends up in `cfgGitRepos`. To keep round trip testing of the parser working
-- we keep `cfgExtraDeps` separate and then provide accessors below that can
-- correctly separate the components.
data StackConfig = StackConfig
  { cfgResolver :: !Text
  , cfgExtraDeps :: ![ConfigExtraDep]
  , cfgLocalDirs :: ![StackLocalDir]
  , cfgGitRepos :: ![StackGitRepo]
  }
  deriving (Eq, Show)

-- | The public StackConfig constructor. Destructors are not provided except
-- via the accessors below.
mkStackConfig :: Text -> [ConfigExtraDep] -> [StackLocalDir] -> [StackGitRepo] -> StackConfig
mkStackConfig = StackConfig

stackResolver :: StackConfig -> Text
stackResolver = cfgResolver

-- | Return only the 'extra-deps' package names and versions from the config.
stackExtraDeps :: StackConfig -> [StackExtraDep]
stackExtraDeps cfg =
  [ dep | ConfigExtraDep dep <- cfgExtraDeps cfg ]

stackLocalDirs :: StackConfig -> [StackLocalDir]
stackLocalDirs = cfgLocalDirs

-- | Return all git repos (with their hashes) from the config, regardless of
-- whether the git repo was listed on 'extra-deps' or 'locations'.
stackGitRepos :: StackConfig -> [StackGitRepo]
stackGitRepos cfg =
  cfgGitRepos cfg
    ++ [ r | ConfigExtraDepRepo r <- cfgExtraDeps cfg ]

-- -----------------------------------------------------------------------------

instance FromYAML StackConfig where
  parseYAML = Yaml.withMap "StackConfig" $ \o ->
    StackConfig
      <$> o .: "resolver"
      <*> ((o .:? "extra-deps") >>= maybe (pure []) (parseArray "StackExtraDep" parseYAML))
      -- The objects in the "packages" can be two different types, so we have
      -- to futz around quite a bit.
      <*> ((o .:? "packages") >>= maybe (pure []) (parseMaybeArray "StackLocalDir" parseMaybeStackLocalDir))
      <*> ((o .:? "packages") >>= maybe (pure []) (parseMaybeArray "StackGitRepo" parseMaybeStackGitRepo))

instance ToJSON StackConfig where
  toJSON cfg =
    Aeson.object
      [ "resolver" .= cfgResolver cfg
      , "extra-deps" .= cfgExtraDeps cfg
      , "packages" .= (fmap toJSON (cfgLocalDirs cfg) ++ fmap toJSON (cfgGitRepos cfg))
      ]

parseArray :: String -> (Node -> Parser a) -> Node -> Parser [a]
parseArray name parser v =
  case v of
    Scalar SNull -> pure []
    _            -> Yaml.withSeq name (mapM parser) v

parseMaybeArray :: String -> (Node -> Parser (Maybe a)) -> Node -> Parser [a]
parseMaybeArray name parser v =
  case v of
    Scalar SNull -> pure []
    _            -> Yaml.withSeq name (mapMaybeM parser) v

data ConfigExtraDep
  = ConfigExtraDep !StackExtraDep
  | ConfigExtraDepRepo !StackGitRepo
  deriving (Eq, Show)

data StackExtraDep
  = StackExtraDep !Text !Version
  deriving (Eq, Show)

instance FromYAML ConfigExtraDep where
  parseYAML (Scalar (SStr s)) = ConfigExtraDep <$> parseStackExtraDep s
  parseYAML (Mapping _ o)     = ConfigExtraDepRepo <$> (StackGitRepo <$> o .: "git" <*> o .: "commit")

  parseYAML invalid = Yaml.typeMismatch "StackExtraDep" invalid

parseStackExtraDep :: Text -> Parser StackExtraDep
parseStackExtraDep str = do
  -- Extra-deps are of the form 'packageName-version' where packageName itself
  -- may have a dash in it.
  let xs = Text.splitOn "-" str
  if List.length xs >= 2
    then pure $ StackExtraDep (Text.intercalate "-" $ init xs) (readVersion $ last xs)
    else fail $ "Can't find version number in extra-dep : " <> Text.unpack str

instance ToJSON ConfigExtraDep where
  toJSON (ConfigExtraDep (StackExtraDep name version)) =
    Aeson.String $ name <> "-" <> Text.pack (showVersion version)
  toJSON (ConfigExtraDepRepo sgr) =
    Aeson.object
      [ "git" .= sgrUrl sgr
      , "commit" .= sgrCommit sgr
      ]

data StackGitRepo = StackGitRepo
  { sgrUrl :: !Text
  , sgrCommit :: !Text
  } deriving (Eq, Show)

instance FromYAML StackGitRepo where
  parseYAML = Yaml.withMap "StackGitRep" $ \o ->
    StackGitRepo
        <$> o .: "git"
        <*> o .: "commit"

instance ToJSON StackGitRepo where
  toJSON sgr =
    Aeson.object
      [ "location" .=
          Aeson.object
            [ "git" .= sgrUrl sgr
            , "commit" .= sgrCommit sgr
            ]
      ]

parseMaybeStackGitRepo :: Node -> Parser (Maybe StackGitRepo)
parseMaybeStackGitRepo v =
  case v of
    Mapping _ o ->
      (o .: "location") >>= \ p ->
        case p of
          Mapping _ q -> Just <$> (StackGitRepo <$> q .: "git" <*> q .: "commit")
          _             -> pure Nothing
    _ -> pure Nothing


newtype StackLocalDir
  = StackLocalDir Text
  deriving (Eq, Show)

instance FromYAML StackLocalDir where
  parseYAML = Yaml.withStr "StackLocalDir" (pure . StackLocalDir)

instance ToJSON StackLocalDir where
  toJSON (StackLocalDir s) = String s

parseMaybeStackLocalDir :: Node -> Parser (Maybe StackLocalDir)
parseMaybeStackLocalDir =
  optional . parseYAML

-- -----------------------------------------------------------------------------

parseStackConfig :: ByteString -> Either JengaError StackConfig
parseStackConfig bs =
  case Yaml.decodeStrict bs of
    Right [cfg] -> Right cfg
    Right []    -> Left $ JengaStackError "empty stack configuration file"
    Right (_:_:_) -> Left $ JengaStackError "Multiple documents encountered in stack configuration file"
    Left s      -> Left $ JengaStackError (Text.pack s)

readStackConfig :: StackFilePath -> EitherT JengaError IO StackConfig
readStackConfig (StackFilePath stackYamlFile) = do
  bs <- handleIOEitherT handler $ BS.readFile stackYamlFile
  hoistEither $ parseStackConfig bs
  where
    handler err
      | isDoesNotExistError err = JengaStackMissing
      | otherwise = JengaStackError $ Text.pack (show err)


renderStackConfig :: StackConfig -> ByteString
renderStackConfig =
  LBS.toStrict . encodePretty' defConfig
