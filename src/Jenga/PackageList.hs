{-# LANGUAGE OverloadedStrings #-}

module Jenga.PackageList
  ( Package (..)
  , PackageList (..)
  , lookupPackages
  ) where

import           Data.Aeson (FromJSON (..), Value (..), (.:))
import           Data.Aeson.Types (Parser, typeMismatch)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Text (Text)

import           Jenga.Types


data PackageList = PackageList
  { ghcVersion :: Text
  , creatDate :: Text
  , resolverName :: Text
  , packageMap :: Map Text Package
  }
  deriving Show

data PackageTemp = PackageTemp -- Not exported
  { _pkgName :: Text
  , _pkgVer :: Version
  , _pkgSyn :: Text
  , _pkgCCore :: Bool
  }

-- Temporary data type. Not exported.
data Snapshot = Snapshot
  { snapshotGhc :: Text
  , snapshotCreated :: Text
  , snapshotName :: Text
  }


instance FromJSON PackageTemp where
  parseJSON (Object v) =
    PackageTemp <$> v .: "name"
            <*> (parseVersion =<< v .: "version")
            <*> v .: "synopsis"
            <*> v .: "isCore"
  parseJSON invalid = typeMismatch "PackageTemp" invalid

parseVersion :: Text -> Parser Version
parseVersion =
  pure . readVersion

instance FromJSON Snapshot where
  parseJSON (Object v) =
    Snapshot <$> v .: "ghc"
              <*> v .: "created"
              <*> v .: "name"
  parseJSON invalid = typeMismatch "Snapshot" invalid

instance FromJSON PackageList where
  parseJSON (Object v) = do
    s <- v .: "snapshot"
    pkgs <- v .: "packages"
    pure $ PackageList (snapshotGhc s) (snapshotCreated s) (snapshotName s) $ mkPackageTempMap pkgs

  parseJSON invalid = typeMismatch "PackageList" invalid

mkPackageTempMap :: [PackageTemp] -> Map Text Package
mkPackageTempMap =
  Map.fromList . fmap convert
  where
    convert (PackageTemp nam ver _ _) =
      (nam, Package nam ver)

lookupPackages :: PackageList -> [Text] -> [Either Text (Text, Package)]
lookupPackages plist deps =
  fmap plookup deps
  where
    pmap = packageMap plist
    plookup k =
      case Map.lookup k pmap of
        Nothing -> Left k
        Just x -> Right (k, x)

