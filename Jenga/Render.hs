{-# LANGUAGE OverloadedStrings #-}
module Jenga.Render
  ( LazyText (..)
  , renderAsCabalConfig
  , renderAsMafiaLock
  ) where

import qualified Data.List as DL
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT

import           Jenga.PackageList


newtype LazyText
  = LazyText { unLazyText :: LT.Text }


renderAsCabalConfig :: [(Text, PackageInfo)] -> LazyText
renderAsCabalConfig pkgs =
  LazyText . LT.fromChunks $ "constraints: " : cabalLines
  where
    cabalLines = DL.concat . DL.intersperse [",\n  "] $ DL.map renderPackage pkgs

renderAsMafiaLock :: [(Text, PackageInfo)] -> LazyText
renderAsMafiaLock pkgs =
  LazyText . LT.fromChunks $ DL.intercalate ["\n"] mafiaLines
  where
    mafiaLines = ["# mafia-lock-file-version: 0"] : DL.map renderPackage pkgs

renderPackage :: (Text, PackageInfo) -> [Text]
renderPackage (name, pkg) =
  [ name, " == ", packageVersion pkg ]

