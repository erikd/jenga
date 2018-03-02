{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forM_, unless)
import           Control.Monad.Extra (concatMapM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT, left, runEitherT)
import           Control.Monad.Trans.Either.Exit (orDie)

import qualified Data.ByteString.Char8 as BS
import           Data.Either (partitionEithers)
import qualified Data.List as DL
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

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
  | Parse StackFilePath


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
  , subCommand "parse"
      ( "Parse the 'stack.yaml' file and dump out the relevant bits as JSON. This is "
      <> "mostly useful for testing and debugging.")
      (Parse <$> stackFilePathP)
  ]
  where
    subCommand :: String -> Doc -> Parser a -> Mod CommandFields a
    subCommand label description parser =
      O.command label (O.info (parser <**> helper) (O.progDescDoc $ Just description))

jengaConfigP :: Parser JengaConfig
jengaConfigP =
  JengaConfig <$> subModulesDirP <*> lockFormatP <*> dropDepsP

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
  fmap (maybe [] (T.split (== ',') . T.pack)) <$> O.optional $ strOption
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

-- -----------------------------------------------------------------------------

commandHandler :: Command -> IO ()
commandHandler cmd =
  orDie renderJengaError $
    case cmd of
      Initialize cfg -> initialize cfg
      Update -> update
      Parse scfg -> dumpStackToJSON scfg

initialize :: JengaConfig -> EitherT JengaError IO ()
initialize jcfg = do
  scfg <- readStackConfig (StackFilePath "stack.yaml")
  ejcfg <- liftIO $ runEitherT readJengaConfig
  case ejcfg of
    Left JengaConfigMissing -> do
      runSetup scfg jcfg
      writeJengaConfig jcfg
    Left err ->
      left err
    Right _ -> do
      liftIO $ T.putStrLn "Found existing Jenga config file and using that."
      runSetup scfg jcfg

update :: EitherT JengaError IO ()
update = do
  scfg <- readStackConfig (StackFilePath "stack.yaml")
  jcfg <- readJengaConfig
  runSetup scfg jcfg

runSetup :: StackConfig -> JengaConfig -> EitherT JengaError IO ()
runSetup stackCfg (JengaConfig modsDir lockFormat dropDeps) = do
  checkModulesDirPath modsDir
  cfiles <- findProjectCabalFiles (StackFilePath "stack.yaml") modsDir
  setupGitSubmodules modsDir $ stackGitRepos stackCfg
  deps <- DL.nub . fmap dependencyName <$> concatMapM readPackageDependencies cfiles
  plist <- getStackageResolverPkgList stackCfg
  subMods <- findSubmodules
  let pkgs = mergePackages (processPackageList deps plist) (stackExtraDeps stackCfg) subMods dropDeps
  liftIO . T.hPutStrLn stderr $ "GHC version: " <> ghcVersion plist
  forM_ cfiles $ \ cabalpath -> do
    writeLockFile (toLockPath lockFormat cabalpath $ ghcVersion plist) pkgs

dumpStackToJSON :: StackFilePath -> EitherT JengaError IO ()
dumpStackToJSON stackFile = do
  scfg <- readStackConfig stackFile
  liftIO . BS.putStrLn $ renderStackConfig scfg

-- -----------------------------------------------------------------------------

checkModulesDirPath :: ModulesDirPath -> EitherT JengaError IO ()
checkModulesDirPath (ModulesDirPath modsDir) = do
  noFiles <- DL.null <$> liftIO (listFiles modsDir)
  unless noFiles $
    left $ JengaSubmodFules modsDir

findProjectCabalFiles :: StackFilePath -> ModulesDirPath -> EitherT JengaError IO [CabalFilePath]
findProjectCabalFiles (StackFilePath stackFile) (ModulesDirPath modsDir) =
  fmap CabalFilePath . filter predicate <$> listDirectoryRecursive (takeDirectory stackFile)
  where
    predicate f = isCabalFile f && not (("." </> modsDir) `DL.isPrefixOf` f)

-- -----------------------------------------------------------------------------

processPackageList :: [Text] -> PackageList -> [Package]
processPackageList deps plist = do
  map snd . snd $ partitionEithers $ lookupPackages plist deps
