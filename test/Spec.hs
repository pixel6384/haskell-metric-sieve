import Sieve
import qualified Data.Text as T
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless)

main :: IO ()
main = do
  let line = T.pack "cpu_usage=45.5"
  let parsed = parseLine line
  let testParsed = case parsed of
        Just (Metric n v) | n == "cpu_usage" && v == 45.5 -> True
        _ -> False

  let sampleMetrics = [Metric "cpu" 10.0, Metric "cpu" 20.0, Metric "mem" 30.0]
  let filtered = filter (filterMetric "cpu" 5.0) sampleMetrics

  let testSum = aggregateSum filtered == 30.0
  let testCount = aggregateCount filtered == 2
  let testAvg = aggregateAvg filtered == 15.0
  let testMax = aggregateMax filtered == 20.0
  let testMin = aggregateMin filtered == 10.0
  
  -- StdDev of [10, 20] is sqrt(((10-15)^2 + (20-15)^2)/2) = sqrt((25+25)/2) = sqrt(25) = 5.0
  let testStdDev = aggregateStdDev filtered == 5.0

  if testParsed && testSum && testCount && testAvg && testMax && testMin && testStdDev
    then putStrLn "All Tests Passed" >> exitSuccess
    else putStrLn "Some Tests Failed" >> exitFailure