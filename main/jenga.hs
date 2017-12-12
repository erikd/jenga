{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forM_)

import           Data.Either (partitionEithers)
import qualified Data.List as DL
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as T

import           Jenga.Cabal
import           Jenga.Git
import           Jenga.Git.SubModules
import           Jenga.HTTP
import           Jenga.PackageList
import           Jenga.Merge
import           Jenga.Render
import           Jenga.Stack

import           Options.Applicative
                        ( CommandFields, Mod, Parser, ParserInfo, ParserPrefs, (<**>)
                        , action, help, helper, info, long, metavar, short, strOption)
import qualified Options.Applicative as O

import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath ((</>), takeDirectory)
import           System.IO (hFlush, stderr, stdout)

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
  = GenCabalFreeze CabalFilePath StackFilePath
  | GenMafiaLock CabalFilePath StackFilePath
  | ModulesDir ModulesDirPath StackFilePath
  | Setup ModulesDirPath StackFilePath


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

  , subCommand "setup"
      ( "Set up the project to be built with Mafia. Specifically that means:\n"
      <> "  * add all git locations in the stack file as git submodules\n"
      <> "  * find all the cabal files that are not submodules and generate a lock file")

      (Setup <$> subModulesDirP <*> stackYamlFileP)
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
    Setup  subModsDir stackFile -> setupProject subModsDir stackFile

genMafiaLock :: CabalFilePath -> StackFilePath -> IO ()
genMafiaLock cabalpath stackpath = do
  deps <- fmap dependencyName <$> readPackageDependencies cabalpath
  hFlush stdout
  mr <- readStackConfig stackpath
  case mr of
    Left err -> putStrLn $ show err
    Right cfg -> do
      plist <- processResolver cfg
      T.hPutStrLn stderr $ "GHC version: " <> ghcVersion plist
      let pkgs = processPackageList deps plist
      writeMafiaLock (toMafiaLockPath cabalpath $ ghcVersion plist) $ mergePackages pkgs (stackExtraDeps cfg) []

genCabalFreeze :: CabalFilePath -> StackFilePath -> IO ()
genCabalFreeze cabalpath stackpath = do
  deps <- fmap dependencyName <$> readPackageDependencies cabalpath
  hFlush stdout
  mr <- readStackConfig stackpath
  case mr of
    Left err -> putStrLn $ show err
    Right cfg -> do
      plist <- processResolver cfg
      T.hPutStrLn stderr $ "GHC version: " <> ghcVersion plist
      let pkgs = processPackageList deps plist
      writeCabalConfig (toCabalFreezePath cabalpath) $ mergePackages pkgs (stackExtraDeps cfg) []


handleSummodules :: ModulesDirPath -> StackFilePath -> IO ()
handleSummodules subModsDir stackFile = do
  mr <- readStackConfig stackFile
  case mr of
    Left err -> putStrLn $ show err
    Right cfg -> setupGitSubmodules subModsDir $ stackGitLocations cfg

setupProject :: ModulesDirPath -> StackFilePath -> IO ()
setupProject subModsDir stackFile = do
  mr <- readStackConfig stackFile
  case mr of
    Left err -> putStrLn $ show err
    Right cfg -> do
      setupGitSubmodules subModsDir $ stackGitLocations cfg
      plist <- processResolver cfg
      subMods <- findSubmodules
      cfiles <- findProjetCabalFiles stackFile subModsDir
      forM_ cfiles $ \ cabalpath -> do
        deps <- fmap dependencyName <$> readPackageDependencies cabalpath
        T.hPutStrLn stderr $ "GHC version: " <> ghcVersion plist
        let pkgs = processPackageList deps plist
        writeMafiaLock (toMafiaLockPath cabalpath $ ghcVersion plist)
            $ mergePackages pkgs (stackExtraDeps cfg) subMods



-- -----------------------------------------------------------------------------

findProjetCabalFiles :: StackFilePath -> ModulesDirPath -> IO [CabalFilePath]
findProjetCabalFiles (StackFilePath stackFile) (ModulesDirPath modsDir) =
  fmap CabalFilePath . filter predicate <$> listDirectoryRecursive (takeDirectory stackFile)
  where
    predicate f = isCabalFile f && not (modsDir `DL.isPrefixOf` f)

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive path = do
  entries    <- fmap (path </>) <$> listDirectory path
  subEntries <- mapM recurse entries
  pure . concat $ entries : subEntries
  where
    recurse entry = do
      isDir <- doesDirectoryExist entry
      if isDir
         then listDirectoryRecursive entry
         else pure []

-- -----------------------------------------------------------------------------

processResolver :: StackConfig -> IO PackageList
processResolver scfg = do
  mpl <- getStackageResolverPkgList scfg
  case mpl of
    Left s -> error $ "Error parse JSON: " ++ s
    Right pl -> pure pl


processPackageList :: [Text] -> PackageList -> [Package]
processPackageList deps plist = do
  map snd . snd $ partitionEithers $ lookupPackages plist deps
