{-# LANGUAGE OverloadedStrings #-}
module Jenga.IO
  ( listDirectoryRecursive
  , listFiles
  ) where

import           Control.Monad (filterM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT, handleIOEitherT, secondEitherT)

import           Jenga.Types

import           System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import           System.FilePath ((</>))

import           System.IO.Error (tryIOError)


-- | Generate a list of all files in the specified directory. The file paths
-- returned are relative to the provided directory.
-- If the staring directory does not exist, a JengaError will be returned.
listDirectoryRecursive :: FilePath -> EitherT JengaError IO [FilePath]
listDirectoryRecursive path = do
  entries <- secondEitherT (fmap (path </>)) $ handleIOEitherT handler (listDirectory path)
  subEntries <- mapM recurse entries
  pure . concat $ entries : subEntries
  where
    handler =
      JengaIOError "listDirectoryRecursive" path
    recurse entry = do
      isDir <- liftIO $ doesDirectoryExist entry
      if isDir
         then listDirectoryRecursive entry
         else pure []

-- | List files in a specified directory.
-- If an IOExceptions occurs (eg directory does not exist) an empty list is returned.
listFiles :: FilePath -> IO [FilePath]
listFiles dir = do
  xs <- either (const []) id <$> tryIOError (listDirectory dir)
  filterM doesFileExist $ fmap (dir </>) xs
