{-# LANGUAGE TemplateHaskell #-}
module Test.Jenga.Stack
  ( tests
  ) where

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Jenga

import           Test.Jenga.Gen


prop_round_trip :: Property
prop_round_trip =
  H.property $ do
    cfg <- H.forAll genStackConfig
    H.tripping cfg renderStackConfig parseStackConfig

tests :: IO Bool
tests =
  H.checkParallel $$(discover)
