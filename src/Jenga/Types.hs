{-# LANGUAGE OverloadedStrings #-}

module Jenga.Types
  ( JengaError (..)
  , LockFormat (..)
  , Package (..)
  , readVersion
  , showVersion

  -- Re-export these from the Cabal library.
  , Version
  , mkVersion
  , versionNumbers
  ) where

import qualified Data.List as DL
import           Data.Text (Text)
import qualified Data.Text as T

import           Distribution.Version (Version, mkVersion, showVersion, versionNumbers)

data LockFormat
  = MafiaLock
  | CabalFreeze
  deriving (Eq, Show)

data Package = Package
  { packageName :: Text
  , packageVersion :: Version
  }
  deriving Show

readVersion :: Text -> Version
readVersion =
  mkVersion . DL.map (read . T.unpack ) . T.split (== '.')


data JengaError
  = JengaConfigMissing
  | JengaConfigError !Text
  | JengaStackError !Text
  deriving (Eq, Show)
