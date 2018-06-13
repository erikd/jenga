{-# LANGUAGE OverloadedStrings #-}

module Jenga.Types
  ( JengaError (..)
  , LockFormat (..)
  , Package (..)
  , readVersion
  , renderJengaError
  , renderVersion

  -- Re-export these from the Cabal library.
  , Version
  , mkVersion
  , versionNumbers
  ) where

import           Control.Exception (IOException)

import qualified Data.List as List
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text

import           Distribution.Pretty (prettyShow)
import           Distribution.Version (Version, mkVersion, versionNumbers)


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
  mkVersion . List.map (read . Text.unpack) . Text.split (== '.')

renderVersion :: Version -> Text
renderVersion = Text.pack . prettyShow

data JengaError
  = JengaConfigMissing
  | JengaConfigError !Text
  | JengaStackMissing
  | JengaStackError !FilePath !Text
  | JengaIOError !Text !FilePath !IOException
  | JengaGitDirMissing
  | JengaParseUrl !Text
  | JengaHttpStatus !Text !String
  | JengaHttpException !String
  | JengaJsonError !String
  | JengaHttpIOError !IOException
  | JengaSubmodFules !FilePath
  | JengaGitError !Text
  deriving (Eq, Show)


renderJengaError :: JengaError -> Text
renderJengaError je =
  case je of
    JengaConfigMissing ->
      "Jenga config file ('.jenga') is missing."
    JengaConfigError t ->
      "Error parsing '.jenga' file: " <> t
    JengaStackMissing ->
      "Missing 'stack.yaml' file."
    JengaStackError fp t ->
      Text.concat [ "Error parsing '", Text.pack fp, "': ", t ]
    JengaIOError f fp ioe ->
      f <> ": Error accessing '" <> Text.pack fp <> "': " <> renderIOException ioe
    JengaGitDirMissing ->
      "Unable to find '.git' directory."
    JengaParseUrl u ->
      "Not able to parse URL: " <> u
    JengaHttpStatus f s ->
      f <> " received HTTP response: " <> Text.pack s
    JengaHttpException s ->
      "HTTP exception: " <> Text.pack s
    JengaJsonError s ->
      "Error reading stackage JSON response: " <> Text.pack s
    JengaHttpIOError ioe ->
      "IOError during HTTP request: " <> renderIOException ioe
    JengaSubmodFules modsDir ->
      "Found files in submodules directory '" <> Text.pack modsDir <> "' which should only have other directories."
    JengaGitError msg ->
      msg

renderIOException :: IOException -> Text
renderIOException =
  Text.pack . show
