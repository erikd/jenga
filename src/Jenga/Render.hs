{-# LANGUAGE OverloadedStrings #-}
module Jenga.Render
  ( LockFilePath (..)
  , writeLockFile
  , toLockPath
  , toMafiaLockPath
  ) where

import qualified Data.List as DL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import           Jenga.PackageList
import           Jenga.Cabal
import           Jenga.Types

import           System.FilePath.Posix ((</>), addExtension, dropExtension, takeDirectory)


data LockFilePath
  = MafiaLockPath !FilePath
  | CabalFreezePath !FilePath


writeLockFile :: LockFilePath -> [Package] -> IO ()
writeLockFile lockPath =
  case lockPath of
    MafiaLockPath mpath -> writeMafiaLock mpath
    CabalFreezePath cpath -> writeCabalConfig cpath

toLockPath :: LockFormat -> CabalFilePath -> Text -> LockFilePath
toLockPath lockFormat cfpath ghcVer =
  case lockFormat of
    MafiaLock -> toMafiaLockPath cfpath ghcVer
    CabalFreeze -> toCabalFreezePath cfpath

writeCabalConfig :: FilePath -> [Package] -> IO ()
writeCabalConfig fpath pkgs =
  LT.writeFile fpath . LT.unlines $ DL.map LT.fromChunks cabalLines
  where
    cabalLines = ["constraints:"] : DL.map renderCabalPackage pkgs

    renderCabalPackage pkg =
      "  " : renderPackage pkg ++ [","]

writeMafiaLock :: FilePath -> [Package] -> IO ()
writeMafiaLock mpath pkgs =
  LT.writeFile mpath . LT.unlines $ DL.map LT.fromChunks mafiaLines
  where
    mafiaLines = ["# mafia-lock-file-version: 0"] : DL.map renderPackage pkgs

renderPackage :: Package -> [Text]
renderPackage pkg =
  [ packageName pkg, " == ", renderVersion (packageVersion pkg) ]
  where
    renderVersion =
      T.pack . DL.intercalate "." . DL.map show . versionNumbers


toMafiaLockPath :: CabalFilePath -> Text -> LockFilePath
toMafiaLockPath (CabalFilePath fp) ghcVer =
  MafiaLockPath . addExtension (dropExtension fp) $ "lock-" ++ T.unpack ghcVer

toCabalFreezePath :: CabalFilePath -> LockFilePath
toCabalFreezePath (CabalFilePath fp) =
  CabalFreezePath $ takeDirectory fp </> "cabal.config"
