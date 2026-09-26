{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Sieve
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)

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

-- | Helper to run the metric pipeline with a range and return a list of matched metrics
getFilteredMetricsRange :: String -> Double -> Double -> IO [Metric]
getFilteredMetricsRange target low high = runConduitRes $ 
  CB.sourceHandle stdin
  .| CT.decodeUtf8
  .| CT.lines
  .| mapC parseLine
  .| concatMapC id
  .| filterC (filterMetricRange (T.pack target) low high)
  .| sinkList

-- | Helper to execute aggregation and print the result
runAgg :: ( [Metric] -> a ) -> String -> String -> String -> IO ()
runAgg agg label target thresholdStr = do
  let threshold = read thresholdStr :: Double
  metrics <- getFilteredMetrics target threshold
  putStrLn $ label ++ ": " ++ show (agg metrics)

-- | Helper to execute aggregation with range and print the result
runAggRange :: ( [Metric] -> a ) -> String -> String -> String -> String -> IO ()
runAggRange agg label target lowStr highStr = do
  let low = read lowStr :: Double
  let high = read highStr :: Double
  metrics <- getFilteredMetricsRange target low high
  putStrLn $ label ++ ": " ++ show (agg metrics)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["avg", target, thresholdStr] -> runAgg aggregateAvg "Average" target thresholdStr
    ["avg", target, low, high] -> runAggRange aggregateAvg "Average" target low high
    ["wavg", target, thresholdStr] -> runAgg aggregateWeightedAvg "Weighted Average" target thresholdStr
    ["wavg", target, low, high] -> runAggRange aggregateWeightedAvg "Weighted Average" target low high
    ["wsum", target, thresholdStr] -> runAgg aggregateWeightedSum "Weighted Sum" target thresholdStr
    ["wsum", target, low, high] -> runAggRange aggregateWeightedSum "Weighted Sum" target low high
    ["sum", target, thresholdStr] -> runAgg aggregateSum "Sum" target thresholdStr
    ["sum", target, low, high] -> runAggRange aggregateSum "Sum" target low high
    ["count", target, thresholdStr] -> runAgg (show . aggregateCount) "Count" target thresholdStr
    ["count", target, low, high] -> runAggRange (show . aggregateCount) "Count" target low high
    ["max", target, thresholdStr] -> runAgg aggregateMax "Max" target thresholdStr
    ["max", target, low, high] -> runAggRange aggregateMax "Max" target low high
    ["min", target, thresholdStr] -> runAgg aggregateMin "Min" target thresholdStr
    ["min", target, low, high] -> runAggRange aggregateMin "Min" target low high
    ["stddev", target, thresholdStr] -> runAgg aggregateStdDev "StdDev" target thresholdStr
    ["stddev", target, low, high] -> runAggRange aggregateStdDev "StdDev" target low high
    ["var", target, thresholdStr] -> runAgg aggregateVariance "Variance" target thresholdStr
    ["var", target, low, high] -> runAggRange aggregateVariance "Variance" target low high
    ["median", target, thresholdStr] -> runAgg aggregateMedian "Median" target thresholdStr
    ["median", target, low, high] -> runAggRange aggregateMedian "Median" target low high
    ["p95", target, thresholdStr] -> runAgg aggregateP95 "P95" target thresholdStr
    ["p95", target, low, high] -> runAggRange aggregateP95 "P95" target low high
    ["p99", target, thresholdStr] -> runAgg aggregateP99 "P99" target thresholdStr
    ["p99", target, low, high] -> runAggRange aggregateP99 "P99" target low high
    [op, target, thresholdStr] | "p" `isPrefixOf` op && length op > 1 -> 
      case reads (drop 1 op) :: [(Double, String)] of
        [(pVal, "")] -> 
          let p = pVal / 100.0
          in runAgg (aggregatePercentile p) ("P" ++ drop 1 op) target thresholdStr
        _ -> putStrLn "Invalid percentile format. Use pXX (e.g. p75)"
    [target, low, high] -> do
      let l = read low :: Double
      let h = read high :: Double
      runConduitRes $ 
        CB.sourceHandle stdin
        .| CT.decodeUtf8
        .| CT.lines
        .| mapC parseLine
        .| concatMapC id
        .| filterC (filterMetricRange (T.pack target) l h)
        .| mapM_C (liftIO . print)
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
  metric-sieve <metric_name> <low> <high>
  metric-sieve avg <metric_name> <threshold>
  metric-sieve wavg <metric_name> <threshold>
  metric-sieve wsum <metric_name> <threshold>
  metric-sieve avg <metric_name> <low> <high>
  metric-sieve wavg <metric_name> <low> <high>
  metric-sieve wsum <metric_name> <low> <high>
  metric-sieve sum <metric_name> <threshold>
  metric-sieve sum <metric_name> <low> <high>
  metric-sieve count <metric_name> <threshold>
  metric-sieve count <metric_name> <low> <high>
  metric-sieve max <metric_name> <threshold>
  metric-sieve max <metric_name> <low> <high>
  metric-sieve min <metric_name> <threshold>
  metric-sieve min <metric_name> <low> <high>
  metric-sieve stddev <metric_name> <threshold>
  metric-sieve stddev <metric_name> <low> <high>
  metric-sieve var <metric_name> <threshold>
  metric-sieve var <metric_name> <low> <high>
  metric-sieve median <metric_name> <threshold>
  metric-sieve median <metric_name> <low> <high>
  metric-sieve p<XX> <metric_name> <threshold> (e.g. p95, p75)
  metric-sieve p95 <metric_name> <threshold>
  metric-sieve p95 <metric_name> <low> <high>
  metric-sieve p99 <metric_name> <threshold>
  metric-sieve p99 <metric_name> <low> <high>"