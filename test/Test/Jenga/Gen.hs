{-# LANGUAGE OverloadedStrings #-}
module Test.Jenga.Gen
  ( genJengaConfig
  , genStackConfig
  ) where

import qualified Data.List as DL
import           Data.Text (Text)
import qualified Data.Text as T

import           Hedgehog (Gen)
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
  where
    localPath =
      ModulesDirPath . joinPath <$> Gen.list (Range.linear 1 5) genFilePath

genStackConfig :: Gen StackConfig
genStackConfig =
  mkStackConfig
    <$> genResolver
    <*> Gen.list (Range.linear 0 5) genStackExtraDep
    <*> Gen.list (Range.linear 0 5) genStackLocalDir
    <*> Gen.list (Range.linear 0 5) genStackGitRepo


genResolver :: Gen Text
genResolver =
  T.pack . DL.take 4 . show . (`div` 100) <$> Gen.int (Range.linear 100 9999)

genStackExtraDep :: Gen ConfigExtraDep
genStackExtraDep =
  Gen.choice
    [ ConfigExtraDep <$> (StackExtraDep <$> genPackageName <*> genPackageVersion)
    , ConfigExtraDepRepo <$> genStackGitRepo
    ]

genStackLocalDir :: Gen StackLocalDir
genStackLocalDir =
  StackLocalDir . T.pack <$> genFilePath

genStackGitRepo :: Gen StackGitRepo
genStackGitRepo =
  StackGitRepo <$> genGitUrl <*> genCommitHash

genGitUrl :: Gen Text
genGitUrl =
  T.concat <$> sequence
    [ Gen.element ["https://github.com/", "git@gitlab.com:", "https://bitbucket.org/"]
    , Gen.element [ "tom", "dick", "harry", "jean-marc" ]
    , Gen.element ["a", "xyz", "this-n-that"]
    ]


genCommitHash :: Gen Text
genCommitHash =
  T.pack <$> Gen.list (Range.singleton 32) (Gen.element "0123456789abcdef")

genFilePath :: Gen FilePath
genFilePath =
  Gen.list (Range.linear 1 10) $
    Gen.frequency
      [ (5, Gen.alphaNum)
      , (1, Gen.element ".-+@$%^& ")
      ]

genPackageName :: Gen Text
genPackageName =
  T.pack . DL.intercalate "-"
    <$> Gen.list (Range.linear 1 3) (Gen.list (Range.linear 1 20) Gen.alphaNum)

genPackageVersion :: Gen Version
genPackageVersion =
  mkVersion <$> Gen.list (Range.linear 1 4) (Gen.int $ Range.linear 1 20)
