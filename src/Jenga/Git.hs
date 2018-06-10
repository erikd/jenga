{-# LANGUAGE OverloadedStrings #-}

module Jenga.Git
  ( setupGitSubmodules
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT, handleIOEitherT)

import qualified Data.Text as Text

import           Jenga.Config
import           Jenga.Git.Command
import           Jenga.Stack
import           Jenga.Types

import           System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import           System.FilePath ((</>))


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
  smp </> Text.unpack (sgrName gitrepo)

updateSubmodule :: FilePath -> StackGitRepo -> EitherT JengaError IO ()
updateSubmodule dir gitrepo = do
  liftIO . putStrLn $ "Updating submodule '" ++ dir ++ "' to commit " ++ Text.unpack (Text.take 10 $ sgrCommit gitrepo)
  gitUpdate dir
  gitCheckoutCommit dir $ Text.unpack (sgrCommit gitrepo)

addSubmodule :: FilePath -> StackGitRepo -> EitherT JengaError IO ()
addSubmodule dir gitrepo = do
  liftIO . putStrLn $ "Adding submodule '" ++ dir ++ "' at commit " ++ Text.unpack (Text.take 10 $ sgrCommit gitrepo)
  gitAddSubmodule dir $ Text.unpack (sgrUrl gitrepo)
  gitCheckoutCommit dir $ Text.unpack (sgrCommit gitrepo)
