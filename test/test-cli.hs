
import           Control.Monad (forM_)
import           System.Directory (listDirectory)
import           System.Process (callProcess)
import           System.IO (BufferMode (..))
import qualified System.IO as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout LineBuffering
  IO.hSetBuffering IO.stderr LineBuffering
  testCliMain ["./dist/build/jenga/jenga"]

testCliMain :: [String] -> IO ()
testCliMain args = do
  tests <- filter (`notElem` ["core", "data"]) <$> listDirectory "test/cli/"
  forM_ tests $ \ t -> callProcess ("test/cli/" ++ t ++ "/run") args
