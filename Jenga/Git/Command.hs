{-# LANGUAGE OverloadedStrings #-}

module Jenga.Git.Command
  ( getAddSubmodule
  , gitCheckoutCommit
  , gitUpdate
  ) where

import           Control.Monad (void)
import           Control.Monad.Trans.Either (runEitherT)

import           Jenga.Git.Process


data JengaError
  = GitProcessError ProcessError

getAddSubmodule :: FilePath -> String -> IO ()
getAddSubmodule dest repo =
  git ["submodule", "add", repo, dest]

gitCheckoutCommit :: String -> IO ()
gitCheckoutCommit hash =
  git ["checkout", hash]

gitUpdate :: IO ()
gitUpdate =
  git ["update"]

git :: [Argument] -> IO ()
git args =
  void wibble
  where
    wibble :: IO (Either JengaError Pass)
    wibble = runEitherT $ call GitProcessError "git" args


