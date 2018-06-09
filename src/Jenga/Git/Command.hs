{-# LANGUAGE OverloadedStrings #-}

module Jenga.Git.Command
  ( gitAddSubmodule
  , gitCheckoutCommit
  , gitHeadHash
  , gitUpdate
  ) where

import           Control.Monad.Trans.Either (EitherT)

import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import           Jenga.Git.Process
import           Jenga.Types


gitAddSubmodule :: FilePath -> String -> EitherT JengaError IO ()
gitAddSubmodule dest repo =
  gitHush ["submodule", "add", "--force", repo, dest]

gitCheckoutCommit :: FilePath -> String -> EitherT JengaError IO ()
gitCheckoutCommit dir hash =
  gitHush ["-C", dir, "checkout", hash]

gitUpdate :: FilePath -> EitherT JengaError IO ()
gitUpdate dir = do
  gitHush ["-C", dir, "fetch"]
  gitHush ["-C", dir, "submodule", "update"]

gitHeadHash :: FilePath -> EitherT JengaError IO Text
gitHeadHash dir =
  gitOut ["-C", dir, "rev-parse", "HEAD"]

gitHush :: [Argument] -> EitherT JengaError IO ()
gitHush args = do
  Hush <- call (JengaGitError . renderProcessError) "git" args
  pure ()

gitOut :: [Argument] -> EitherT JengaError IO Text
gitOut args =
  Text.takeWhile (not . isSpace)
    . Text.decodeUtf8With Text.lenientDecode
    . unOut
    <$> call (JengaGitError . renderProcessError) "git" args
