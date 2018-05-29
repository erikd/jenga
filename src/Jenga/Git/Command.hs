{-# LANGUAGE OverloadedStrings #-}

module Jenga.Git.Command
  ( gitAddSubmodule
  , gitCheckoutCommit
  , gitUpdate
  ) where

import           Control.Monad.Trans.Either (EitherT)

import           Jenga.Git.Process
import           Jenga.Types


gitAddSubmodule :: FilePath -> String -> EitherT JengaError IO ()
gitAddSubmodule dest repo =
  git ["submodule", "add", "--force", repo, dest]

gitCheckoutCommit :: FilePath -> String -> EitherT JengaError IO ()
gitCheckoutCommit dir hash =
  git ["-C", dir, "checkout", hash]

gitUpdate :: FilePath -> EitherT JengaError IO ()
gitUpdate dir = do
  git ["-C", dir, "fetch"]
  git ["-C", dir, "submodule", "update"]

git :: [Argument] -> EitherT JengaError IO ()
git args = do
  Hush <- call (JengaGitError . renderProcessError) "git" args
  pure ()
