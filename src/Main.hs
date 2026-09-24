{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Sieve
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["avg", target, thresholdStr] -> do
      let threshold = read thresholdStr :: Double
      metrics <- runConduitRes $ 
        CB.sourceHandle stdin
        .| CT.decodeUtf8
        .| CT.lines
        .| mapC parseLine
        .| concatMapC id
        .| filterC (filterMetric (T.pack target) threshold)
        .| sinkList
      putStrLn $ "Average: " ++ show (aggregateMetrics metrics)
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
  metric-sieve avg <metric_name> <threshold"