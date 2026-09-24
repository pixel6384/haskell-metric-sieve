import Sieve
import qualified Data.Text as T
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  let line = T.pack "cpu_usage=45.5"
  let parsed = parseLine line
  case parsed of
    Just (Metric n v) | n == "cpu_usage" && v == 45.5 -> putStrLn "Test Passed" >> exitSuccess
    _ -> putStrLn "Test Failed" >> exitFailure