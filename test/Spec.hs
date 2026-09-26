import Sieve
import qualified Data.Text as T
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless)

main :: IO ()
main = do
  let line = T.pack "cpu_usage=45.5"
  let parsed = parseLine line
  let testParsed = case parsed of
        Just (Metric n v w) | n == "cpu_usage" && v == 45.5 && w == 1.0 -> True
        _ -> False

  let lineW = T.pack "cpu_usage=45.5,2.0"
  let parsedW = parseLine lineW
  let testParsedW = case parsedW of
        Just (Metric n v w) | n == "cpu_usage" && v == 45.5 && w == 2.0 -> True
        _ -> False

  let sampleMetrics = [Metric "cpu" 10.0 1.0, Metric "cpu" 20.0 2.0, Metric "mem" 30.0 1.0]
  let filtered = filter (filterMetric "cpu" 5.0) sampleMetrics

  let testSum = aggregateSum filtered == 30.0
  let testWeightedSum = aggregateWeightedSum filtered == (10.0*1.0 + 20.0*2.0)
  let testCount = aggregateCount filtered == 2
  let testAvg = aggregateAvg filtered == 15.0
  let testWeightedAvg = aggregateWeightedAvg filtered == (10.0*1.0 + 20.0*2.0) / 3.0
  let testMax = aggregateMax filtered == 20.0
  let testMin = aggregateMin filtered == 10.0
  
  -- Variance of [10, 20] is ((10-15)^2 + (20-15)^2)/2 = (25+25)/2 = 25.0
  let testVariance = aggregateVariance filtered == 25.0
  -- StdDev of [10, 20] is sqrt(25.0) = 5.0
  let testStdDev = aggregateStdDev filtered == 5.0

  -- P95 of [10, 20] should be 20.0 (index = ceiling(0.95*2)-1 = 2-1 = 1)
  let testP95 = aggregateP95 filtered == 20.0

  if testParsed && testParsedW && testSum && testWeightedSum && testCount && testAvg && testWeightedAvg && testMax && testMin && testVariance && testStdDev && testP95
    then putStrLn "All Tests Passed" >> exitSuccess
    else putStrLn "Some Tests Failed" >> exitFailure