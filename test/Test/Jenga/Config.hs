{-# LANGUAGE TemplateHaskell #-}
module Test.Jenga.Config
  ( tests
  ) where

import           Hedgehog (Gen, Property, discover)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Jenga

import           System.FilePath (joinPath)

genJengaConfig :: Gen JengaConfig
genJengaConfig =
  JengaConfig <$> localPath <*> Gen.bool
  where
    localPath =
      joinPath <$> Gen.list (Range.linear 1 5) pathName

    pathName =
      Gen.list (Range.linear 1 10) $
        Gen.frequency
          [ (5, Gen.alphaNum)
          , (1, Gen.element ".-+@$%^& ")
          ]

prop_round_trip :: Property
prop_round_trip =
  H.property $ do
    cfg <- H.forAll genJengaConfig
    H.tripping cfg renderJengaConfig parseJengaConfig

tests :: IO Bool
tests =
  H.checkParallel $$(discover)
