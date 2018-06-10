{-# LANGUAGE TemplateHaskell #-}
module Test.Jenga.Config
  ( tests
  ) where

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Jenga

import           Test.Jenga.Gen

prop_round_trip :: Property
prop_round_trip =
  H.withTests 500 . H.property $ do
    cfg <- H.forAll genJengaConfig
    H.tripping cfg renderJengaConfig parseJengaConfig

tests :: IO Bool
tests =
  H.checkParallel $$(discover)
