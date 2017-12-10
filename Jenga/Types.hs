{-# LANGUAGE OverloadedStrings #-}

module Jenga.Types
  ( Package (..)
  ) where

import           Data.Text (Text)

data Package = Package
  { packageName :: Text
  , packageVersion :: Text
  }
  deriving Show
