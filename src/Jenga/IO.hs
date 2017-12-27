{-# LANGUAGE OverloadedStrings #-}
module Jenga.IO
  ( listDirectoryRecursive
  , listFiles
  ) where

import           Control.Monad (filterM)

import           System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import           System.FilePath ((</>))

import           System.IO.Error (tryIOError)

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

listFiles :: FilePath -> IO [FilePath]
listFiles dir = do
  xs <- either (const []) id <$> tryIOError (listDirectory dir)
  filterM doesFileExist $ fmap (dir </>) xs


