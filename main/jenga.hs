{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (unless)

import           Data.Either (partitionEithers)
import qualified Data.List as DL
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.Lazy.IO as T

import           Jenga.Cabal
import           Jenga.HTTP
import           Jenga.PackageList
import           Jenga.Render
import           Jenga.Stack

import           Options.Applicative (Parser, execParser, flag, help, info, long, short, strOption)

import           System.IO (hPutStrLn, stderr)


main :: IO ()
main =
  execParser (info pCommand mempty) >>= process

-- -----------------------------------------------------------------------------

data OutputFormat
  = OutputCabal
  | OutputMafia

data Command = Command FilePath OutputFormat

pCommand :: Parser Command
pCommand = Command <$> cabalFileP <*> outputFormatP

cabalFileP :: Parser FilePath
cabalFileP = strOption
  (  short 'i'
  <> long "input"
  <> help "The input cabal file."
  )

outputFormatP :: Parser OutputFormat
outputFormatP =
  flag OutputCabal OutputMafia
    (  short 'm'
    <> long "mafia"
    <> help "Render the output as a mafia lock file (rendering as a cabal.config is the default)."
    )

-- -----------------------------------------------------------------------------

process :: Command -> IO ()
process (Command cabalpath fmt) = do
  deps <- fmap dependencyName <$> readPackageDependencies cabalpath
  mr <- readResolver
  case mr of
    Nothing -> putStrLn "Not able to find resolver version in 'stack.yaml' file."
    Just r -> processResolver fmt deps r

processResolver :: OutputFormat -> [Text] -> StackResolver -> IO ()
processResolver fmt deps sr = do
  mpl <- getStackageResolverPkgList sr
  case mpl of
    Left s -> putStrLn $ "Error parse JSON: " ++ s
    Right pl -> processPackageList fmt deps pl


processPackageList :: OutputFormat -> [Text] -> PackageList -> IO ()
processPackageList fmt deps plist = do
  let (missing, found) = partitionEithers $ lookupPackages plist deps
  unless (DL.null missing) $
    reportMissing missing
  T.putStrLn . unLazyText $
    case fmt of
      OutputCabal -> renderAsCabalConfig found
      OutputMafia -> renderAsMafiaLock found


reportMissing :: [Text] -> IO ()
reportMissing [] = putStrLn "No missing packages found."
reportMissing xs =
  hPutStrLn stderr $ "The packages " ++ show xs ++ " could not be found in the specified stack resolver data."
