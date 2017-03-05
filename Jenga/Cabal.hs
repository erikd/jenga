module Jenga.Cabal
  ( dependencyName
  , readPackageDependencies
  ) where

-- You would thing that since the Cabal file exposes its cabal parser you would
-- think it would be a simple matter to extract the list of dependencies.
-- Unfortunately its much more work than it should be. See:
-- https://hackage.haskell.org/package/Cabal-1.24.2.0/docs/Distribution-PackageDescription.html#v:buildDepends

import           Data.Text (Text)
import qualified Data.Text as T

import           Distribution.Package (Dependency (..), PackageName (..))
import           Distribution.PackageDescription
                    ( Benchmark, CondTree (..), ConfVar, Executable, GenericPackageDescription (..)
                    , Library, TestSuite
                    )
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity (normal)


readPackageDependencies :: FilePath -> IO [Dependency]
readPackageDependencies fpath = do
  genpkg <- readPackageDescription normal fpath
  pure $ extractLibraryDeps (condLibrary genpkg)
        ++ extractExecutableDeps (condExecutables genpkg)
        ++ extractTestSuiteDeps (condTestSuites genpkg)
        ++ extractBenchmarkDeps (condBenchmarks genpkg)


dependencyName :: Dependency -> Text
dependencyName (Dependency (PackageName name) _) = T.pack name


extractLibraryDeps :: Maybe (CondTree ConfVar [Dependency] Library) -> [Dependency]
extractLibraryDeps Nothing = []
extractLibraryDeps (Just x) = condTreeConstraints x

extractExecutableDeps :: [(String, CondTree ConfVar [Dependency] Executable)] -> [Dependency]
extractExecutableDeps = concatMap (condTreeConstraints . snd)

extractTestSuiteDeps :: [(String, CondTree ConfVar [Dependency] TestSuite)] -> [Dependency]
extractTestSuiteDeps = concatMap (condTreeConstraints . snd)

extractBenchmarkDeps :: [(String, CondTree ConfVar [Dependency] Benchmark)] -> [Dependency]
extractBenchmarkDeps = concatMap (condTreeConstraints . snd)
