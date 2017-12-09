{-# LANGUAGE OverloadedStrings #-}
module Jenga.Render
  ( CabalFreezePath (..)
  , MafiaLockPath (..)
  , writeCabalConfig
  , writeMafiaLock
  , toCabalFreezePath
  , toMafiaLockPath
  ) where

import qualified Data.List as DL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import           Jenga.PackageList
import           Jenga.Cabal

import           System.FilePath.Posix ((</>), addExtension, dropExtension, takeDirectory)


newtype MafiaLockPath
  = MafiaLockPath { unMafiaLockPath :: FilePath }

newtype CabalFreezePath
  = CabalFreezePath { unCabalFreezePath :: FilePath }


writeCabalConfig :: CabalFreezePath -> [Package] -> IO ()
writeCabalConfig (CabalFreezePath fpath) pkgs =
  LT.writeFile fpath . LT.fromChunks $ "constraints: " : cabalLines
  where
    cabalLines = DL.concat . DL.intersperse [",\n  "] $ DL.map renderPackage pkgs

writeMafiaLock :: MafiaLockPath -> [Package] -> IO ()
writeMafiaLock (MafiaLockPath mpath) pkgs =
  LT.writeFile mpath . LT.unlines $ DL.map LT.fromChunks mafiaLines
  where
    mafiaLines = ["# mafia-lock-file-version: 0"] : DL.map renderPackage pkgs

renderPackage :: Package -> [Text]
renderPackage pkg =
  [ packageName pkg, " == ", packageVersion pkg ]


toMafiaLockPath :: CabalFilePath -> Text -> MafiaLockPath
toMafiaLockPath (CabalFilePath fp) ghcVer =
  MafiaLockPath . addExtension (dropExtension fp) $ "lock-" ++ T.unpack ghcVer

toCabalFreezePath :: CabalFilePath -> CabalFreezePath
toCabalFreezePath (CabalFilePath fp) =
  CabalFreezePath $ takeDirectory fp </> "cabal.config"
