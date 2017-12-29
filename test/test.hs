
import           Control.Monad (unless)

import           System.IO (BufferMode (..), hSetBuffering, stdout, stderr)
import           System.Exit (exitFailure)

import qualified Test.Jenga.Config
import qualified Test.Jenga.Stack

main :: IO ()
main = runTests
  [ Test.Jenga.Config.tests
  , Test.Jenga.Stack.tests
  ]

runTests :: [IO Bool] -> IO ()
runTests tests = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence tests
  unless (and results)
    exitFailure
