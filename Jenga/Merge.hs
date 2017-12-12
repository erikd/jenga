module Jenga.Merge
  ( mergePackages
  ) where

import qualified Data.List as DL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as DM
import           Data.Text (Text)

import           Jenga.Git.SubModules
import           Jenga.Stack
import           Jenga.Types


-- Merge the packages from the resolver, the extra-deps and the git
-- submodules.
mergePackages :: [Package] -> [StackExtraDep] -> [GitSubmodule] -> [Package]
mergePackages pkgs deps submods =
  -- Start with a `Map PkgName PkgVersion` generated from the packages listed
  -- by the stack resolver.
  let pkgMap0 = DM.fromList $ DL.map (\p -> (packageName p, packageVersion p)) pkgs

  -- Now add the extra-dep packages from the stack file. Any package name in
  -- pkgMap0 may have its version number overridden here.
      pkgMap1 = DL.foldl' insertExtraDep pkgMap0 deps
  -- Finally add any packages that were listed as git repositories in the stack
  -- file. Again, these versions will override any existing package versions.
      pkgMap2 = DL.foldl' insertPackage pkgMap1 $ DL.map smPackage submods
  in DL.map mkPackage $ DM.toList pkgMap2
  where
    insertExtraDep :: Map Text Text -> StackExtraDep -> Map Text Text
    insertExtraDep pmap dep =
      DM.insert (sedName dep) (sedVersion dep) pmap

    insertPackage :: Map Text Text -> Package -> Map Text Text
    insertPackage pmap pkg =
      DM.insert (packageName pkg) (packageVersion pkg) pmap

    mkPackage :: (Text, Text) -> Package
    mkPackage (nam, ver) = Package nam ver
