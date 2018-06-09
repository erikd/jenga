{-# LANGUAGE OverloadedStrings #-}

module Jenga.Git
  ( setupGitSubmodules
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT, handleIOEitherT, hoistEither)

import qualified Data.Text as Text

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
  dir <- hoistEither $ buildSubmoduleDir smp gitrepo
  exists <- liftIO $ doesDirectoryExist dir
  if exists
    then updateSubmodule dir gitrepo
    else addSubmodule dir gitrepo


buildSubmoduleDir :: ModulesDirPath -> StackGitRepo -> Either JengaError FilePath
buildSubmoduleDir (ModulesDirPath smp) gitrepo =
  case parseURI (Text.unpack $ sgrUrl gitrepo) of
    Nothing -> Left $ JengaParseUrl (sgrUrl gitrepo)
    Just uri ->
      case split (== '/') $ uriPath uri of
        ["",  _, name] -> Right $ smp </> dropExtension name
        _ -> Left $ JengaParseUrl (sgrUrl gitrepo)

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
  liftIO . putStrLn $ "Updating submodule '" ++ dir ++ "' to commit " ++ Text.unpack (Text.take 10 $ sgrCommit gitrepo)
  gitUpdate dir
  gitCheckoutCommit dir $ Text.unpack (sgrCommit gitrepo)

addSubmodule :: FilePath -> StackGitRepo -> EitherT JengaError IO ()
addSubmodule dir gitrepo = do
  liftIO . putStrLn $ "Adding submodule '" ++ dir ++ "' at commit " ++ Text.unpack (Text.take 10 $ sgrCommit gitrepo)
  gitAddSubmodule dir $ Text.unpack (sgrUrl gitrepo)
  gitCheckoutCommit dir $ Text.unpack (sgrCommit gitrepo)
