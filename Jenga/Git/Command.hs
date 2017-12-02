{-# LANGUAGE OverloadedStrings #-}

module Jenga.Git.Command
  ( getAddSubmodule
  , gitCheckoutCommit
  , gitUpdate
  ) where



getAddSubmodule :: FilePath -> String -> IO ()
getAddSubmodule dest repo =
  putStrLn $ "git submodule add " ++ repo ++ " " ++ dest

gitCheckoutCommit :: String -> IO ()
gitCheckoutCommit hash =
  putStrLn $ "git checkout " ++ hash

gitUpdate :: IO ()
gitUpdate =
  putStrLn "git update"
