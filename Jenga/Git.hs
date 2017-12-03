{-# LANGUAGE OverloadedStrings #-}

module Jenga.Git
  ( setupGitSubmodules
  ) where

import qualified Data.Text as T

import           Jenga.Git.Command
import           Jenga.Stack

import           Network.URI (parseURI, uriPath)

import           System.Directory (doesDirectoryExist, withCurrentDirectory)
import           System.FilePath ((</>), dropExtension)

newtype ModulesDirPath = ModulesDirPath FilePath


setupGitSubmodules :: ModulesDirPath ->  [StackGitRepo] -> IO ()
setupGitSubmodules smp =
  mapM_ (setupSubmodule smp)


setupSubmodule :: ModulesDirPath -> StackGitRepo -> IO ()
setupSubmodule smp gitrepo = do
  let dir = buildSubmoduleDir smp gitrepo
  exists <- doesDirectoryExist dir
  if exists
    then updateSubmodule dir gitrepo
    else addSubmodule dir gitrepo



buildSubmoduleDir :: ModulesDirPath -> StackGitRepo -> FilePath
buildSubmoduleDir (ModulesDirPath smp) gitrepo =
  case parseURI (T.unpack $ sgrUrl gitrepo) of
    Nothing -> error $ "Not able to parse " ++ show (sgrUrl gitrepo)
    Just uri ->
      case split (/= '/') $ uriPath uri of
        ["",  _, name] -> smp </> dropExtension name
        _ -> error $ "buildSubmoduleDir: Bad git repo user/project: '" ++ uriPath uri ++ "'."

split :: Eq a => (a -> Bool) -> [a] -> [[a]]
split p =
  splitter
  where
    splitter [] = []
    splitter xs =
      case break p xs of
        (h, []) -> [h]
        (h, t) -> h : splitter (drop 1 t)


updateSubmodule :: FilePath -> StackGitRepo -> IO ()
updateSubmodule dir gitrepo =
  withCurrentDirectory dir $ do
    gitUpdate
    gitCheckoutCommit $ T.unpack (sgrCommit gitrepo)

addSubmodule :: FilePath -> StackGitRepo -> IO ()
addSubmodule dir gitrepo =
  getAddSubmodule dir $ T.unpack (sgrUrl gitrepo)
