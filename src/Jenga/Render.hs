{-# LANGUAGE OverloadedStrings #-}
module Jenga.Render
  ( LockFilePath (..)
  , writeLockFile
  , toLockPath
  , toMafiaLockPath
  ) where

import           Control.Monad.Trans.Either (EitherT, handleIOEitherT)

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText

import           Jenga.PackageList
import           Jenga.Cabal
import           Jenga.Types

import           System.FilePath.Posix ((</>), addExtension, dropExtension, takeDirectory)


data LockFilePath
  = MafiaLockPath !FilePath
  | CabalFreezePath !FilePath


writeLockFile :: LockFilePath -> [Package] -> EitherT JengaError IO ()
writeLockFile lockPath =
  case lockPath of
    MafiaLockPath mpath -> writeMafiaLock mpath
    CabalFreezePath cpath -> writeCabalConfig cpath

toLockPath :: LockFormat -> CabalFilePath -> Text -> LockFilePath
toLockPath lockFormat cfpath ghcVer =
  case lockFormat of
    MafiaLock -> toMafiaLockPath cfpath ghcVer
    CabalFreeze -> toCabalFreezePath cfpath

writeCabalConfig :: FilePath -> [Package] -> EitherT JengaError IO ()
writeCabalConfig fpath pkgs =
  -- Generating the cabal freeze file that cabal will actually accept is a
  -- pain in the neck.
  writeFileEitherT fpath $ LazyText.fromChunks (cabalLines ++ ["\n"])
  where
    cabalLines =
      case pkgs of
        [] -> ["constraints:"]
        (x:xs) -> List.intersperse ",\n "
                    $ Text.concat ("constraints: " : renderPackage x)
                        : List.map (Text.concat . renderPackage) xs


writeMafiaLock :: FilePath -> [Package] -> EitherT JengaError IO ()
writeMafiaLock mpath pkgs =
  writeFileEitherT mpath . LazyText.unlines $ List.map LazyText.fromChunks mafiaLines
  where
    mafiaLines = ["# mafia-lock-file-version: 0"] : List.map renderPackage pkgs

renderPackage :: Package -> [Text]
renderPackage pkg =
  [ packageName pkg, " == ", renderVersion (packageVersion pkg) ]

toMafiaLockPath :: CabalFilePath -> Text -> LockFilePath
toMafiaLockPath (CabalFilePath fp) ghcVer =
  MafiaLockPath . addExtension (dropExtension fp) $ "lock-" ++ Text.unpack ghcVer

toCabalFreezePath :: CabalFilePath -> LockFilePath
toCabalFreezePath (CabalFilePath fp) =
  CabalFreezePath $ takeDirectory fp </> "cabal.config"


writeFileEitherT :: FilePath -> LazyText.Text -> EitherT JengaError IO ()
writeFileEitherT path =
  handleIOEitherT handler . LazyText.writeFile path
  where
    handler =
      JengaIOError "writeFileEitherT" path
