{-# LANGUAGE OverloadedStrings #-}

module Jenga.PackageList
  ( PackageInfo (..)
  , PackageList (..)
  ) where

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as DM

import           Data.Text (Text)


data PackageList = PackageList
  { ghcVersion :: Text
  , creatDate :: Text
  , resolverName :: Text
  , packageList :: Map Text PackageInfo
  }
  deriving Show

data Package = Package
  { _pkgName :: Text
  , _pkgVer :: Text
  , _pkgSyn :: Text
  , _pkgCCore :: Bool
  }

data PackageInfo = PackageInfo
  { packageVersion :: Text
  , packageSynopsis :: Text
  , packageCore :: Bool
  }
  deriving Show

-- Temporary data type. Not exported.
data Snapshot = Snapshot
  { ghc :: Text
  , created :: Text
  , name :: Text
  }


instance FromJSON Package where
  parseJSON (Object v) =
    Package <$> v .: "name"
            <*> v .: "version"
            <*> v .: "synopsis"
            <*> v .: "isCore"
  parseJSON invalid = typeMismatch "Package" invalid


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
    pure $ PackageList (ghc s) (created s) (name s) $ packageMap pkgs

  parseJSON invalid = typeMismatch "PackageList" invalid

packageMap :: [Package] -> Map Text PackageInfo
packageMap =
  DM.fromList . fmap convert
  where
    convert (Package nam ver syn core) =
      (nam, PackageInfo ver syn core)
