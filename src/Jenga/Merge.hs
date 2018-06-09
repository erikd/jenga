module Jenga.Merge
  ( mergePackages
  ) where

import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

import           Jenga.Git.SubModules
import           Jenga.Stack
import           Jenga.Types


-- Merge the packages from the resolver, the extra-deps and the git
-- submodules.
mergePackages :: [Package] -> [StackExtraDep] -> [GitSubmodule] -> [Text] -> [Package]
mergePackages pkgs deps submods dropDeps =
  -- Start with a `Map PkgName PkgVersion` generated from the packages listed
  -- by the stack resolver.
  let pkgMap0 = Map.fromList $ List.map (\p -> (packageName p, packageVersion p)) pkgs

  -- Now add the extra-dep packages from the stack file. Any package name in
  -- pkgMap0 may have its version number overridden here.
      pkgMap1 = List.foldl' insertExtraDep pkgMap0 deps
  -- Finally add any packages that were listed as git repositories in the stack
  -- file. Again, these versions will override any existing package versions.
      pkgMap2 = List.foldl' insertPackage pkgMap1 $ List.map smPackage submods

  -- Now remove all packages listed as drop dependencies.
      pkgMap3 = List.foldl' dropPackage pkgMap2 dropDeps
  in List.map (uncurry Package) $ Map.toList pkgMap3
  where
    insertExtraDep :: Map Text Version -> StackExtraDep -> Map Text Version
    insertExtraDep pmap sed =
      case sed of
        StackExtraDep name version -> Map.insert name version pmap

    insertPackage :: Map Text Version -> Package -> Map Text Version
    insertPackage pmap pkg =
      Map.insert (packageName pkg) (packageVersion pkg) pmap

    dropPackage :: Map Text Version -> Text -> Map Text Version
    dropPackage  pmap name =
      Map.delete name pmap
