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
  git ["submodule", "add", repo, dest]

gitCheckoutCommit :: String -> IO ()
gitCheckoutCommit hash =
  git ["checkout", hash]

gitUpdate :: IO ()
gitUpdate = do
  git ["fetch"]
  git ["submodule", "update"]

git :: [Argument] -> IO ()
git args =
  either (error . show) (const $ pure ()) =<< wibble
  where
    wibble :: IO (Either JengaError Hush)
    wibble = runEitherT $ call GitProcessError "git" args


