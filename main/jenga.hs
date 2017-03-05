{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (unless)

import           Data.Either (partitionEithers)
import qualified Data.List as DL
import           Data.Text (Text)
import qualified Data.Text.Lazy.IO as T

import           Jenga.Cabal
import           Jenga.HTTP
import           Jenga.PackageList
import           Jenga.Render
import           Jenga.Stack

import           System.IO (hPutStrLn, stderr)
import           System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [cf] -> process cf
    _ -> usageExit


usageExit :: IO ()
usageExit =
  putStrLn $ "\nUsage: jenga <cabal file>\n"

process :: FilePath -> IO ()
process cabalFile = do
  deps <- fmap dependencyName <$> readPackageDependencies cabalFile
  mr <- readResolver
  case mr of
    Nothing -> putStrLn "Not able to find resolver version in 'stack.yaml' file."
    Just r -> processResolver deps r

processResolver :: [Text] -> StackResolver -> IO ()
processResolver deps sr = do
  mpl <- getStackageResolverPkgList sr
  case mpl of
    Left s -> putStrLn $ "Error parse JSON: " ++ s
    Right pl -> processPackageList deps pl


processPackageList :: [Text] -> PackageList -> IO ()
processPackageList deps plist = do
  let (missing, found) = partitionEithers $ lookupPackages plist deps
  unless (DL.null missing) $
    reportMissing missing
  T.putStrLn . unLazyText $ renderAsCabalConfig found


reportMissing :: [Text] -> IO ()
reportMissing [] = putStrLn "No missing packages found."
reportMissing xs =
  hPutStrLn stderr $ "The packages " ++ show xs ++ " could not be found in the specified stack resolver data."
