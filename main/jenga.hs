{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forM_, unless)
import           Control.Monad.Extra (concatMapM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT, left, runEitherT)
import           Control.Monad.Trans.Either.Exit (orDie)

import qualified Data.ByteString.Char8 as BS
import           Data.Either (partitionEithers)
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Jenga

import           Options.Applicative
                        ( CommandFields, Mod, Parser, ParserInfo, ParserPrefs, (<**>)
                        , help, helper, long, metavar, short, strOption)
import qualified Options.Applicative as O

import           System.FilePath ((</>), takeDirectory)
import           System.IO (stderr)

import           Text.PrettyPrint.ANSI.Leijen (Doc)

main :: IO ()
main =
  O.customExecParser p opts >>= commandHandler
  where
    opts :: ParserInfo Command
    opts = O.info (helper <*> pCommand)
      ( O.fullDesc <> O.header "jenga - A tool for helping to build Stack projects"
      )
    p :: ParserPrefs
    p = O.prefs O.showHelpOnEmpty

-- -----------------------------------------------------------------------------

data Command
  = Initialize JengaConfig
  | Update
  | ParseStack StackFilePath
  | ParseJenga FilePath


pCommand :: Parser Command
pCommand = O.subparser $ mconcat
  [ subCommand "init"
      ( "Initialize a project to be built with Mafia. Specifically that means:\n"
      <> "  * Add all git locations in the stack file as git submodules\n"
      <> "  * Find all the cabal files that are not submodules and generate a lock file for each\n"
      <> "This command assumes that it is being run in a Git repo and that that 'stack.yaml' file is in the top level directory of the Git repo."
      <> "This command will also generate a '.jenga' file in the top level directory.")
      (Initialize <$> jengaConfigP)

  , subCommand "update"
      ( "Update a previously initialized a project. A typical use case would be doing a"
      <> "git pull on a project (which may have changed stack resolvers, exxtra dependencies"
      <> "or other things) and want to build is the new updated project. This sub command"
      <> "expects a '.jenga' file in the same directory as the 'stack.yaml' file.")
      (pure Update)
  , subCommand "parse-stack"
      ( "Parse a 'stack.yaml' file and dump out the relevant bits as JSON. This is "
      <> "mostly useful for testing and debugging.")
      (ParseStack <$> stackFilePathP)
  , subCommand "parse-jenga"
      ( "Parse the '.jenga' file and dump out the relevant bits as JSON. This is "
      <> "mostly useful for testing and debugging.")
      (ParseJenga <$> jengaFilePathP)
  ]
  where
    subCommand :: String -> Doc -> Parser a -> Mod CommandFields a
    subCommand label description parser =
      O.command label (O.info (parser <**> helper) (O.progDescDoc $ Just description))

jengaConfigP :: Parser JengaConfig
jengaConfigP =
  JengaConfig <$> subModulesDirP <*> lockFormatP <*> dropDepsP <*> pure []

subModulesDirP :: Parser ModulesDirPath
subModulesDirP =
  fmap (ModulesDirPath . fromMaybe "lib") <$> O.optional $ strOption
  (  short 'm'
  <> long "modules"
  <> metavar "MODULES_DIRECTORY"
  <> help "The optional directory in which to put git submodules (defaults to '/lib/')."
  )

dropDepsP :: Parser [Text]
dropDepsP =
  fmap (maybe [] (Text.split (== ',') . Text.pack)) <$> O.optional $ strOption
  (  short 'd'
  <> long "drop-deps"
  <> metavar "DROP_DEPENDENCIES"
  <> help "Comma separated list of dependencies to drop from the lock/freeze file. These will be saved to the '.jenga' file."
  )

lockFormatP :: Parser LockFormat
lockFormatP =  O.flag MafiaLock CabalFreeze
  (  short 'f'
  <> long "cabal-freeze"
  <> help "Generate cabal freeze file (cabal.config) instead of mafia lock file."
  )

stackFilePathP :: Parser StackFilePath
stackFilePathP =
  fmap (StackFilePath . fromMaybe "stack.yaml") <$> O.optional $ strOption
  (  short 's'
  <> long "stack-file"
  <> metavar "STACK_FILE"
  <> help "The 'stack.yaml' file. (defaults to './stack.yaml')."
  )

jengaFilePathP :: Parser FilePath
jengaFilePathP =
  fmap (fromMaybe ".jenga") <$> O.optional $ strOption
  (  short 'j'
  <> long "jenga-file"
  <> metavar "JENGA_FILE"
  <> help "The '.jenga' file. (defaults to './.jenga')."
  )

-- -----------------------------------------------------------------------------

commandHandler :: Command -> IO ()
commandHandler cmd =
  orDie renderJengaError $
    case cmd of
      Initialize cfg -> initialize cfg
      Update -> update
      ParseStack scfg -> dumpStackToJSON scfg
      ParseJenga jcfg -> dumpJengaToJSON jcfg

initialize :: JengaConfig -> EitherT JengaError IO ()
initialize jcfg = do
  scfg <- readStackConfig (StackFilePath "stack.yaml")
  ejcfg <- liftIO $ runEitherT readJengaConfig
  case ejcfg of
    Left JengaConfigMissing -> do
      runSetup scfg jcfg
    Left err ->
      left err
    Right _ -> do
      liftIO $ Text.putStrLn "Found existing Jenga config file and using that."
      runUpdate scfg jcfg

update :: EitherT JengaError IO ()
update = do
  scfg <- readStackConfig (StackFilePath "stack.yaml")
  jcfg <- readJengaConfig
  runUpdate scfg jcfg

runUpdate :: StackConfig -> JengaConfig -> EitherT JengaError IO ()
runUpdate scfg jcfg = do
  runSetup scfg jcfg
  let (newConfig, oldSubmods) = mergeGitSubmodules jcfg $ stackGitRepos scfg
  forM_ oldSubmods $ \ sm -> gitRemove (jsmPath sm)
  writeJengaConfig newConfig

runSetup :: StackConfig -> JengaConfig -> EitherT JengaError IO ()
runSetup scfg jcfg = do
  checkModulesDirPath (jcModulesDirPath jcfg)
  cfiles <- findProjectCabalFiles (StackFilePath "stack.yaml") (jcModulesDirPath jcfg)
  setupGitSubmodules (jcModulesDirPath jcfg) $ stackGitRepos scfg
  deps <- List.nub . fmap dependencyName <$> concatMapM readPackageDependencies cfiles
  plist <- getStackageResolverPkgList scfg
  subMods <- findSubmodules
  let pkgs = mergePackages (processPackageList deps plist) (stackExtraDeps scfg) subMods (jcDropDeps jcfg)
  liftIO . Text.hPutStrLn stderr $ "GHC version: " <> ghcVersion plist
  forM_ cfiles $ \ cabalpath -> do
    writeLockFile (toLockPath (jcMafiaLock jcfg) cabalpath $ ghcVersion plist) pkgs
  writeJengaConfig $ fst (mergeGitSubmodules jcfg $ stackGitRepos scfg)

dumpStackToJSON :: StackFilePath -> EitherT JengaError IO ()
dumpStackToJSON stackFile = do
  scfg <- readStackConfig stackFile
  liftIO . BS.putStrLn $ renderStackConfig scfg

dumpJengaToJSON :: FilePath -> EitherT JengaError IO ()
dumpJengaToJSON file = do
  jcfg <- readJengaConfigFrom file
  liftIO . BS.putStrLn $ renderJengaConfig jcfg

-- -----------------------------------------------------------------------------

checkModulesDirPath :: ModulesDirPath -> EitherT JengaError IO ()
checkModulesDirPath (ModulesDirPath modsDir) = do
  noFiles <- List.null <$> liftIO (listFiles modsDir)
  unless noFiles $
    left $ JengaSubmodFules modsDir

-- Find cabal files belong to this project, which specifically means
-- not cabal files in git submodules, or in a cabal sandbox or the
-- new style sandboxes.
findProjectCabalFiles :: StackFilePath -> ModulesDirPath -> EitherT JengaError IO [CabalFilePath]
findProjectCabalFiles (StackFilePath stackFile) (ModulesDirPath modsDir) =
  fmap CabalFilePath . filter predicate <$> listDirectoryRecursive (takeDirectory stackFile)
  where
    predicate f =
      isCabalFile f
        && not (("." </> modsDir) `List.isPrefixOf` f)
        && not (".cabal-sandbox/" `List.isInfixOf` f)
        && not ("dist-newstyle/" `List.isInfixOf` f)

-- -----------------------------------------------------------------------------

processPackageList :: [Text] -> PackageList -> [Package]
processPackageList deps plist = do
  map snd . snd $ partitionEithers $ lookupPackages plist deps
