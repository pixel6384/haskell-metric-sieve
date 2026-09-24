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

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["avg", target, thresholdStr] -> do
      let threshold = read thresholdStr :: Double
      metrics <- getFilteredMetrics target threshold
      putStrLn $ "Average: " ++ show (aggregateAvg metrics)
    ["sum", target, thresholdStr] -> do
      let threshold = read thresholdStr :: Double
      metrics <- getFilteredMetrics target threshold
      putStrLn $ "Sum: " ++ show (aggregateSum metrics)
    ["count", target, thresholdStr] -> do
      let threshold = read thresholdStr :: Double
      metrics <- getFilteredMetrics target threshold
      putStrLn $ "Count: " ++ show (aggregateCount metrics)
    ["max", target, thresholdStr] -> do
      let threshold = read thresholdStr :: Double
      metrics <- getFilteredMetrics target threshold
      putStrLn $ "Max: " ++ show (aggregateMax metrics)
    ["min", target, thresholdStr] -> do
      let threshold = read thresholdStr :: Double
      metrics <- getFilteredMetrics target threshold
      putStrLn $ "Min: " ++ show (aggregateMin metrics)
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
  metric-sieve min <metric_name> <threshold"