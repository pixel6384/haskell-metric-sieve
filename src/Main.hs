{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Sieve
import Control.Monad.IO.Class (liftIO)

-- | Helper to run the metric pipeline and return a list of matched metrics
getFilteredMetrics :: String -> Double -> IO [Metric]
getFilteredMetrics target threshold = runConduitRes $ 
  CB.sourceHandle stdin
  .| CT.decodeUtf8
  .| CT.lines
  .| mapC parseLine
  .| concatMapC id
  .| filterC (filterMetric (T.pack target) threshold)
  .| sinkList

-- | Helper to execute aggregation and print the result
runAgg :: ( [Metric] -> a ) -> String -> String -> Double -> IO ()
runAgg agg label target thresholdStr = do
  let threshold = read thresholdStr :: Double
  metrics <- getFilteredMetrics target threshold
  putStrLn $ label ++ ": " ++ show (agg metrics)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["avg", target, thresholdStr] -> runAgg aggregateAvg "Average" target thresholdStr
    ["sum", target, thresholdStr] -> runAgg aggregateSum "Sum" target thresholdStr
    ["count", target, thresholdStr] -> runAgg (show . aggregateCount) "Count" target thresholdStr
    ["max", target, thresholdStr] -> runAgg aggregateMax "Max" target thresholdStr
    ["min", target, thresholdStr] -> runAgg aggregateMin "Min" target thresholdStr
    ["stddev", target, thresholdStr] -> runAgg aggregateStdDev "StdDev" target thresholdStr
    ["median", target, thresholdStr] -> runAgg aggregateMedian "Median" target thresholdStr
    ["p95", target, thresholdStr] -> runAgg aggregateP95 "P95" target thresholdStr
    ["p99", target, thresholdStr] -> runAgg aggregateP99 "P99" target thresholdStr
    [target, thresholdStr] -> do
      let threshold = read thresholdStr :: Double
      runConduitRes $ 
        CB.sourceHandle stdin
        .| CT.decodeUtf8
        .| CT.lines
        .| mapC parseLine
        .| concatMapC id
        .| filterC (filterMetric (T.pack target) threshold)
        .| mapM_C (liftIO . print)
    _ -> putStrLn "Usage:
  metric-sieve <metric_name> <threshold>
  metric-sieve avg <metric_name> <threshold>
  metric-sieve sum <metric_name> <threshold>
  metric-sieve count <metric_name> <threshold>
  metric-sieve max <metric_name> <threshold>
  metric-sieve min <metric_name> <threshold>
  metric-sieve stddev <metric_name> <threshold>
  metric-sieve median <metric_name> <threshold>
  metric-sieve p95 <metric_name> <threshold>
  metric-sieve p99 <metric_name> <threshold"