{-# LANGUAGE OverloadedStrings #-}
module Jenga.IO
  ( listDirectoryRecursive
  ) where

import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath ((</>))


listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive path = do
  -- putStrLn $ "listDirectoryRecursive " ++ path
  entries    <- fmap (path </>) <$> listDirectory path
  subEntries <- mapM recurse entries
  pure . concat $ entries : subEntries
  where
    recurse entry = do
      isDir <- doesDirectoryExist entry
      if isDir
         then listDirectoryRecursive entry
         else pure []
