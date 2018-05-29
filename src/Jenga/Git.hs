{-# LANGUAGE OverloadedStrings #-}

module Jenga.Git
  ( setupGitSubmodules
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT, handleIOEitherT)

import qualified Data.Text as T

import           Jenga.Config
import           Jenga.Git.Command
import           Jenga.Stack
import           Jenga.Types

import           Network.URI (parseURI, uriPath)

import           System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import           System.FilePath ((</>), dropExtension)


setupGitSubmodules :: ModulesDirPath ->  [StackGitRepo] -> EitherT JengaError IO ()
setupGitSubmodules smp =
  mapM_ (setupSubmodule smp)


setupSubmodule :: ModulesDirPath -> StackGitRepo -> EitherT JengaError IO ()
setupSubmodule smp gitrepo = do
  handleIOEitherT (JengaIOError "setupSubmodule" (unModulesDirPath smp)) $ do
    createDirectoryIfMissing False $ unModulesDirPath smp
  let dir = buildSubmoduleDir smp gitrepo
  exists <- liftIO $ doesDirectoryExist dir
  if exists
    then updateSubmodule dir gitrepo
    else addSubmodule dir gitrepo


buildSubmoduleDir :: ModulesDirPath -> StackGitRepo -> FilePath
buildSubmoduleDir (ModulesDirPath smp) gitrepo =
  case parseURI (T.unpack $ sgrUrl gitrepo) of
    Nothing -> error $ "Not able to parse " ++ show (sgrUrl gitrepo)
    Just uri ->
      case split (== '/') $ uriPath uri of
        ["",  _, name] -> smp </> dropExtension name
        xs -> error $ "buildSubmoduleDir: Bad git repo user/project: " ++ show xs

split :: (a -> Bool) -> [a] -> [[a]]
split p =
  splitter
  where
    splitter [] = []
    splitter xs =
      case break p xs of
        (h, []) -> [h]
        (h, t) -> h : splitter (drop 1 t)


updateSubmodule :: FilePath -> StackGitRepo -> EitherT JengaError IO ()
updateSubmodule dir gitrepo = do
  liftIO . putStrLn $ "Updating submodule '" ++ dir ++ "' to commit " ++ T.unpack (T.take 10 $ sgrCommit gitrepo)
  gitUpdate dir
  gitCheckoutCommit dir $ T.unpack (sgrCommit gitrepo)

addSubmodule :: FilePath -> StackGitRepo -> EitherT JengaError IO ()
addSubmodule dir gitrepo = do
  liftIO . putStrLn $ "Adding submodule '" ++ dir ++ "' at commit " ++ T.unpack (T.take 10 $ sgrCommit gitrepo)
  gitAddSubmodule dir $ T.unpack (sgrUrl gitrepo)
  gitCheckoutCommit dir $ T.unpack (sgrCommit gitrepo)
