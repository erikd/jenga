{-# LANGUAGE OverloadedStrings #-}
module Jenga.Git.SubModules
  ( GitSubmodule (..)
  , findSubmodules
  , isCabalFile
  ) where

import           Control.Monad (forM)
import           Control.Monad.Catch (handleIOError)
import           Control.Monad.Extra (concatMapM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT, left)

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


findSubmodules :: EitherT JengaError IO [GitSubmodule]
findSubmodules = do
  topdir <- findGitTopLevel
  concatMapM mkGitSubmodule =<< liftIO (readSubmodules topdir)
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

    -- This should only be needed if the '.gitmodules' file does not exist.
    -- In that case we silently drop the error and return an empty list.
    handler :: IOError -> IO [a]
    handler _ = pure []



findGitTopLevel :: EitherT JengaError IO FilePath
findGitTopLevel =
  loop =<< liftIO getCurrentDirectory
  where
    loop "/" = left JengaGitDirMissing
    loop cur = do
      exists <- liftIO . doesDirectoryExist $ cur </> ".git"
      if exists
        then pure cur
        else loop (takeDirectory cur)


