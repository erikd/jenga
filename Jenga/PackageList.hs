{-# LANGUAGE OverloadedStrings #-}

module Jenga.PackageList
  ( Package (..)
  , PackageList (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)


data PackageList = PackageList
  { ghcVersion :: Text
  , creatDate :: Text
  , resolverName :: Text
  , packageList :: [Package]
  }
  deriving Show

data Package = Package
  { packageName :: Text
  , packageVersion :: Text
  , synopsis :: Text
  , isCore :: Bool
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
    pure $ PackageList (ghc s) (created s) (name s) pkgs

  parseJSON invalid = typeMismatch "PackageList" invalid
