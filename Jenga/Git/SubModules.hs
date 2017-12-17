{-# LANGUAGE OverloadedStrings #-}

module Jenga.Git.SubModules
  ( GitSubmodule (..)
  , findSubmodules
  , isCabalFile
  ) where

import           Control.Monad (forM)
import           Control.Monad.Catch (handleIOError)
import           Control.Monad.Extra (concatMapM)

import           Data.Char (isSpace)
import qualified Data.List as DL

import           Jenga.Cabal
import           Jenga.IO
import           Jenga.Types

import           System.Directory (doesDirectoryExist, getCurrentDirectory)
import           System.FilePath ((</>), takeDirectory, takeExtension)


data GitSubmodule = GitSubmodule
  { smDirectory :: FilePath
  , smCabalFile :: CabalFilePath
  , smPackage :: Package
  }


findSubmodules :: IO [GitSubmodule]
findSubmodules = do
  topdir <- findGitTopLevel
  concatMapM mkGitSubmodule =<< readSubmodules topdir
  where
    mkGitSubmodule dir = do
      files <- filter isCabalFile <$> listDirectoryRecursive dir
      forM files $ \ x -> do
            let cf = CabalFilePath x
            GitSubmodule dir cf <$> readPackageFromCabalFile cf

isCabalFile :: FilePath -> Bool
isCabalFile file =
  takeExtension file == ".cabal"

readSubmodules :: FilePath -> IO [FilePath]
readSubmodules fpath =
  handleIOError handler $ do
    xs <- lines <$> readFile (fpath </> ".gitmodules")
    pure $ map clean $ filter isPathLine xs
  where
    isPathLine xs = DL.isPrefixOf "path" $ dropWhile isSpace xs

    clean = dropWhile isSpace . drop 1 . dropWhile (/= '=')

    handler :: IOError -> IO [a]
    handler e = print e >> pure []



findGitTopLevel :: IO FilePath
findGitTopLevel =
  loop =<< getCurrentDirectory
  where
    loop "/" = do error "findGitTopLevel failed"
    loop cur = do
      exists <- doesDirectoryExist $ cur </> ".git"
      if exists
        then pure cur
        else loop (takeDirectory cur)


