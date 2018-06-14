{-# LANGUAGE OverloadedStrings #-}
module Test.Jenga.Gen
  ( genJengaConfig
  , genStackConfig
  ) where

import qualified Data.List as DL
import           Data.Text (Text)
import qualified Data.Text as Text

import           Hedgehog (Gen)
import qualified Hedgehog.Corpus as Corpus
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Jenga

import           System.FilePath (joinPath)

genJengaConfig :: Gen JengaConfig
genJengaConfig =
  JengaConfig
    <$> localPath
    <*> Gen.element [ MafiaLock, CabalFreeze ]
    <*> Gen.list (Range.linear 0 5) genPackageName
    <*> Gen.list (Range.linear 0 5) genJengaSubmodule
  where
    localPath =
      ModulesDirPath . joinPath <$> Gen.list (Range.linear 1 5) pathName

    pathName =
      Gen.list (Range.linear 1 10) $
        Gen.frequency
          [ (5, Gen.alphaNum)
          , (1, Gen.element ".-+@$%^& ")
          ]

genStackConfig :: Gen StackConfig
genStackConfig =
  mkStackConfig
    <$> genResolver
    <*> Gen.list (Range.linear 0 5) genStackExtraDep
    <*> Gen.list (Range.linear 0 5) genStackLocalDir
    <*> Gen.list (Range.linear 0 5) genStackGitRepo


genResolver :: Gen Text
genResolver =
  Text.pack . DL.take 4 . show . (`div` 100) <$> Gen.int (Range.linear 100 9999)

genStackExtraDep :: Gen ConfigExtraDep
genStackExtraDep =
  Gen.choice
    [ ConfigExtraDep <$> (StackExtraDep <$> genPackageName <*> genPackageVersion)
    , ConfigExtraDepRepo <$> fmap simplify genStackGitRepo
    ]
  where
    -- Git repos listed as an "extra-dep" cannot specify "subdirs" or "extra-dep"
    simplify (StackGitRepo u n c _ _) = StackGitRepo u n c [] True

genStackLocalDir :: Gen StackLocalDir
genStackLocalDir =
  StackLocalDir . Text.pack <$> genFilePath

genStackGitRepo :: Gen StackGitRepo
genStackGitRepo = do
  (location, owner, repoName, ext) <- getGitRepoUrlParts
  StackGitRepo (Text.concat [location, owner, "/", repoName, ext]) repoName
    <$> genCommitHash
    <*> Gen.list (Range.linear 0 5) (Gen.element Corpus.southpark)
    <*> Gen.bool

getGitRepoUrlParts :: Gen (Text, Text, Text, Text)
getGitRepoUrlParts =
  (,,,) <$> Gen.element ["https://github.com/", "git@github.com:", "https://bitbucket.org/"]
        <*> Gen.element Corpus.simpsons
        <*> Gen.element ["a", "xyz", "this-n-that"]
        <*> Gen.element [mempty, ".git"]

genGitUrl :: Gen Text
genGitUrl = do
  (location, owner, repoName, ext) <- getGitRepoUrlParts
  pure $ Text.concat [location, owner, "/", repoName, ext]

genCommitHash :: Gen Text
genCommitHash =
  Text.pack <$> Gen.list (Range.singleton 32) (Gen.element "0123456789abcdef")

genFilePath :: Gen FilePath
genFilePath =
  Gen.list (Range.linear 1 10) $
    Gen.frequency
      [ (5, Gen.alphaNum)
      , (1, Gen.element ".-+@$%^& ")
      ]

genPackageName :: Gen Text
genPackageName =
  Text.pack . DL.intercalate "-"
    <$> Gen.list (Range.linear 1 3) (Gen.list (Range.linear 1 20) Gen.alphaNum)

genPackageVersion :: Gen Version
genPackageVersion =
  mkVersion <$> Gen.list (Range.linear 1 4) (Gen.int $ Range.linear 1 20)

genJengaSubmodule :: Gen JengaSubmodule
genJengaSubmodule =
  JengaSubmodule
    <$> genGitUrl
    <*> genFilePath
    <*> genCommitHash
