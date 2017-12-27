{-# LANGUAGE TemplateHaskell #-}
module Test.Jenga.Config
  ( tests
  ) where

import qualified Data.List as DL
import qualified Data.Text as T

import           Hedgehog (Gen, Property, discover)
import qualified Hedgehog as H
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
      joinPath <$> Gen.list (Range.linear 1 5) pathName

    pathName =
      Gen.list (Range.linear 1 10) $
        Gen.frequency
          [ (5, Gen.alphaNum)
          , (1, Gen.element ".-+@$%^& ")
          ]

    genPackageName =
      T.pack . DL.intercalate "-"
        <$> Gen.list (Range.linear 1 3) (Gen.list (Range.linear 1 20) Gen.alphaNum)


prop_round_trip :: Property
prop_round_trip =
  H.property $ do
    cfg <- H.forAll genJengaConfig
    H.tripping cfg renderJengaConfig parseJengaConfig

tests :: IO Bool
tests =
  H.checkParallel $$(discover)
