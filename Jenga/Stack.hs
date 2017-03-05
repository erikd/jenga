module Jenga.Stack
  ( readResolver
  ) where

-- Read the resolver from the stack.yaml file in the current directory.
-- This is unashamedly savage.

import           Data.Char (isSpace)
import qualified Data.List as DL

import           Jenga.HTTP


readResolver :: IO (Maybe StackResolver)
readResolver = -- Is this the *only* file name?
  extract <$> readFile "stack.yaml"


extract :: String -> Maybe StackResolver
extract xs =
  case filter (DL.isPrefixOf "resolver:") (lines xs) of
    [x] -> Just .StackResolver $ cleanup x
    _ -> Nothing
  where
    cleanup = DL.takeWhile (not . isSpace)
            . DL.dropWhile (isSpace)
            . DL.drop 1
            . DL.dropWhile (/= ':')
