{-# LANGUAGE OverloadedStrings #-}

module Jenga.Git.Command
  ( gitAddSubmodule
  , gitCheckoutCommit
  , gitUpdate
  ) where

import           Control.Monad.Trans.Either (runEitherT)

import           Jenga.Git.Process


data JengaError
  = GitProcessError ProcessError
  deriving (Show)


gitAddSubmodule :: FilePath -> String -> IO ()
gitAddSubmodule dest repo =
  git ["submodule", "add", "--force", repo, dest]

gitCheckoutCommit :: FilePath -> String -> IO ()
gitCheckoutCommit dir hash =
  git ["-C", dir, "checkout", hash]

gitUpdate :: FilePath -> IO ()
gitUpdate dir = do
  git ["-C", dir, "fetch"]
  git ["-C", dir, "submodule", "update"]

git :: [Argument] -> IO ()
git args =
  either (error . show) (const $ pure ()) =<< wibble
  where
    wibble :: IO (Either JengaError Hush)
    wibble = runEitherT $ call GitProcessError "git" args


