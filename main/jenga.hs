{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (unless)

import           Data.Either (partitionEithers)
import qualified Data.List as DL
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as T

import           Jenga.Cabal
import           Jenga.Git
import           Jenga.HTTP
import           Jenga.PackageList
import           Jenga.Render
import           Jenga.Stack

import           Options.Applicative
                        ( CommandFields, Mod, Parser, ParserInfo, ParserPrefs, (<**>)
                        , action, help, helper, info, long, metavar, short, strOption)
import qualified Options.Applicative as O


import           System.IO (hPutStrLn, stderr)
import           System.IO (hFlush, stdout)

main :: IO ()
main =
  O.customExecParser p opts >>= commandHandler
  where
    opts :: ParserInfo Command
    opts = info (helper <*> pCommand)
      ( O.fullDesc <> O.header "jenaga - A tool for helping to build Stack projects"
      )
    p :: ParserPrefs
    p = O.prefs O.showHelpOnEmpty

-- -----------------------------------------------------------------------------


data Command
  = GenMafiaLock CabalFilePath StackFilePath
  | GenCabalFreeze CabalFilePath StackFilePath
  | ModulesDir ModulesDirPath StackFilePath


pCommand :: Parser Command
pCommand = O.subparser $ mconcat
  [ subCommand "genlock"
      "Generate a mafia lock file for the given stack.yaml and cabal file."
      (GenMafiaLock <$> cabalFileP <*> stackYamlFileP)
  , subCommand "genfreeze"
      "Generate a cabal freeze file for the given stack.yaml and cabal file."
      (GenCabalFreeze <$> cabalFileP <*> stackYamlFileP)
  , subCommand "submods"
      "Read the given stack.yaml file, extract all git extra deprendencies, add them, and checkout the specified git hash."
      (ModulesDir <$> subModulesDirP <*> stackYamlFileP)
  ]
  where
    subCommand :: String -> String -> Parser a -> Mod CommandFields a
    subCommand label description parser =
      O.command label (info (parser <**> helper) (O.progDesc description))

subModulesDirP :: Parser ModulesDirPath
subModulesDirP = ModulesDirPath <$> strOption
  (  short 'm'
  <> long "modules"
  <> metavar "MODULES_DIRECTORY"
  <> help "The directory in which to put git submodules."
  <> action "directory"
  )

cabalFileP :: Parser CabalFilePath
cabalFileP = CabalFilePath <$> strOption
  (  short 'c'
  <> long "cabal"
  <> metavar "INPUT_CABAL_FILE"
  <> help "The input cabal file."
  <> action "file"
  )

stackYamlFileP :: Parser StackFilePath
stackYamlFileP = StackFilePath <$> strOption
  (  short 's'
  <> long "stack"
  <> metavar "INPUT_STACK_YAML_FILE"
  <> help "The input stack.yaml file."
  <> action "file"
  )

-- -----------------------------------------------------------------------------

commandHandler :: Command -> IO ()
commandHandler cmd =
  case cmd of
    GenMafiaLock cabalFile stackFile -> genMafiaLock cabalFile stackFile
    GenCabalFreeze cabalFile stackFile -> genCabalFreeze cabalFile stackFile
    ModulesDir subModsDir stackFile -> handleSummodules subModsDir stackFile

genMafiaLock :: CabalFilePath -> StackFilePath -> IO ()
genMafiaLock cabalpath stackpath = do
  deps <- fmap dependencyName <$> readPackageDependencies cabalpath
  hFlush stdout
  mr <- readStackConfig stackpath
  case mr of
    Left err -> putStrLn $ show err
    Right cfg -> do
      plist <- processResolver cfg
      processPackageList deps plist >>= writeMafiaLock (toMafiaLockPath cabalpath $ ghcVersion plist)

genCabalFreeze :: CabalFilePath -> StackFilePath -> IO ()
genCabalFreeze cabalpath stackpath = do
  deps <- fmap dependencyName <$> readPackageDependencies cabalpath
  hFlush stdout
  mr <- readStackConfig stackpath
  case mr of
    Left err -> putStrLn $ show err
    Right cfg -> do
      plist <- processResolver cfg
      processPackageList deps plist >>= writeCabalConfig (toCabalFreezePath cabalpath)


handleSummodules :: ModulesDirPath -> StackFilePath -> IO ()
handleSummodules subModsDir stackFile = do
  mr <- readStackConfig stackFile
  case mr of
    Left err -> putStrLn $ show err
    Right cfg -> setupGitSubmodules subModsDir $ stackGitLocations cfg

processResolver :: StackConfig -> IO PackageList
processResolver scfg = do
  mpl <- getStackageResolverPkgList scfg
  case mpl of
    Left s -> error $ "Error parse JSON: " ++ s
    Right pl -> pure pl


processPackageList :: [Text] -> PackageList -> IO [(Text, PackageInfo)]
processPackageList deps plist = do
  let (missing, found) = partitionEithers $ lookupPackages plist deps
  unless (DL.null missing) $
    reportMissing missing
  T.hPutStrLn stderr $ "GHC version: " <> ghcVersion plist
  pure found


reportMissing :: [Text] -> IO ()
reportMissing [] = putStrLn "No missing packages found."
reportMissing xs =
  hPutStrLn stderr $ "The packages " ++ show xs ++ " could not be found in the specified stack resolver data."
