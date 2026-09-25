{-# LANGUAGE OverloadedStrings #-}
module Sieve (Metric(..), parseLine, filterMetric, filterMetricRange, aggregateAvg, aggregateSum, aggregateCount, aggregateMax, aggregateMin, aggregateStdDev, aggregateMedian, aggregateP95, aggregateP99, aggregatePercentile, aggregateWeightedAvg) where

import qualified Data.Text as T
import Data.List (sort)

data Metric = Metric
  { name   :: T.Text
  , value  :: Double
  , weight :: Double
  } deriving (Show, Eq)

-- | Parses a line in format "metric_name=value" or "metric_name=value,weight", ignoring surrounding whitespace
parseLine :: T.Text -> Maybe Metric
parseLine line = 
  case T.splitOn "=" line of
    [n, vPart] -> 
      let cleanN = T.strip n
          vSplit = T.splitOn "," vPart
      in case vSplit of
           [vStr] -> 
             case reads (T.unpack (T.strip vStr)) of
               [(val, "")] -> Just $ Metric cleanN val 1.0
               _            -> Nothing
           [vStr, wStr] -> 
             case (reads (T.unpack (T.strip vStr)), reads (T.unpack (T.strip wStr))) of
               ([(val, "")], [(w, "")]) -> Just $ Metric cleanN val w
               _                         -> Nothing
           _ -> Nothing
    _      -> Nothing

-- | Predicate to filter metrics by name and threshold (greater than)
filterMetric :: T.Text -> Double -> Metric -> Bool
filterMetric targetName threshold m = 
  name m == targetName && value m > threshold

-- | Predicate to filter metrics by name and value range (inclusive)
filterMetricRange :: T.Text -> Double -> Double -> Metric -> Bool
filterMetricRange targetName low high m = 
  name m == targetName && value m >= low && value m <= high

-- | Sum of filtered metrics
aggregateSum :: [Metric] -> Double
aggregateSum = sum . map value

-- | Count of filtered metrics
aggregateCount :: [Metric] -> Int
aggregateCount = length

-- | Average of filtered metrics
aggregateAvg :: [Metric] -> Double
aggregateAvg [] = 0
aggregateAvg ms = aggregateSum ms / fromIntegral (aggregateCount ms)

-- | Weighted Average of filtered metrics
aggregateWeightedAvg :: [Metric] -> Double
aggregateWeightedAvg [] = 0
aggregateWeightedAvg ms = 
  let weightedSum = sum [value m * weight m | m <- ms]
      totalWeight = sum [weight m | m <- ms]
  in if totalWeight == 0 then 0 else weightedSum / totalWeight

-- | Maximum value of filtered metrics
aggregateMax :: [Metric] -> Double
aggregateMax [] = 0
aggregateMax ms = maximum (map value ms)

-- | Minimum value of filtered metrics
aggregateMin :: [Metric] -> Double
aggregateMin [] = 0
aggregateMin ms = minimum (map value ms)

-- | Standard Deviation of filtered metrics
aggregateStdDev :: [Metric] -> Double
aggregateStdDev [] = 0
aggregateStdDev [_] = 0
aggregateStdDev ms = 
  let values = map value ms
      avg = aggregateAvg ms
      variance = sum [(v - avg)**2 | v <- values] / fromIntegral (length values)
  in sqrt variance

-- | Generic percentile aggregation
aggregatePercentile :: Double -> [Metric] -> Double
aggregatePercentile _ [] = 0
aggregatePercentile p ms = 
  let sorted = sort (map value ms)
      len = length sorted
      index = ceiling (p * fromIntegral len) - 1
  in sorted !! max 0 index

-- | Median value of filtered metrics
aggregateMedian :: [Metric] -> Double
aggregateMedian [] = 0
aggregateMedian ms = 
  let sorted = sort (map value ms)
      len = length sorted
      mid = len `div` 2
  in if odd len
     then sorted !! mid
     else (sorted !! (mid - 1) + sorted !! mid) / 2

-- | 95th Percentile of filtered metrics
aggregateP95 :: [Metric] -> Double
aggregateP95 = aggregatePercentile 0.95

-- | 99th Percentile of filtered metrics
aggregateP99 :: [Metric] -> Double
aggregateP99 = aggregatePercentile 0.99