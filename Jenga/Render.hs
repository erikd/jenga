{-# LANGUAGE OverloadedStrings #-}
module Jenga.Render
  ( LazyText (..)
  , MafiaLockPath (..)
  , renderAsCabalConfig
  , renderAsMafiaLock
  , toMafiaLockPath
  ) where

import qualified Data.List as DL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import           Jenga.PackageList
import           Jenga.Cabal

import           System.FilePath.Posix (addExtension, dropExtension)


newtype MafiaLockPath
  = MafiaLockPath { unMafiaLockPath :: FilePath }

newtype LazyText
  = LazyText { unLazyText :: LT.Text }


renderAsCabalConfig :: [(Text, PackageInfo)] -> LazyText
renderAsCabalConfig pkgs =
  LazyText . LT.fromChunks $ "constraints: " : cabalLines
  where
    cabalLines = DL.concat . DL.intersperse [",\n  "] $ DL.map renderPackage pkgs

renderAsMafiaLock :: MafiaLockPath -> [(Text, PackageInfo)] -> IO ()
renderAsMafiaLock (MafiaLockPath mpath) pkgs =
  LT.writeFile mpath . LT.fromChunks $ DL.intercalate ["\n"] mafiaLines
  where
    mafiaLines = ["# mafia-lock-file-version: 0"] : DL.map renderPackage pkgs

renderPackage :: (Text, PackageInfo) -> [Text]
renderPackage (name, pkg) =
  [ name, " == ", packageVersion pkg ]


toMafiaLockPath :: CabalFilePath -> Text -> MafiaLockPath
toMafiaLockPath (CabalFilePath fp) ghcVer =
  MafiaLockPath . addExtension (dropExtension fp) $ "lock-" ++ T.unpack ghcVer
