{-# LANGUAGE OverloadedStrings #-}
module Sieve (Metric(..), parseLine, filterMetric, aggregateAvg, aggregateSum, aggregateCount, aggregateMax, aggregateMin) where

import qualified Data.Text as T
import Data.Maybe (mapMaybe)

data Metric = Metric
  { name  :: T.Text
  , value :: Double
  } deriving (Show, Eq)

-- | Parses a line in format "metric_name=value"
parseLine :: T.Text -> Maybe Metric
parseLine line = 
  case T.splitOn "=" line of
    [n, v] -> case reads (T.unpack v) of
                 [(val, "")] -> Just $ Metric n val
                 _            -> Nothing
    _      -> Nothing

-- | Predicate to filter metrics by name and threshold
filterMetric :: T.Text -> Double -> Metric -> Bool
filterMetric targetName threshold m = 
  name m == targetName && value m > threshold

-- | Sum of filtered metrics
aggrSum :: [Metric] -> Double
aggrSum = sum . map value

-- | Count of filtered metrics
aggrCount :: [Metric] -> Int
aggrCount = length

-- | Average of filtered metrics
aggrAvg :: [Metric] -> Double
aggrAvg [] = 0
aggrAvg ms = aggrSum ms / fromIntegral (aggrCount ms)

-- | Maximum value of filtered metrics
aggrMax :: [Metric] -> Double
aggrMax [] = 0
aggrMax ms = maximum (map value ms)

-- | Minimum value of filtered metrics
aggrMin :: [Metric] -> Double
aggrMin [] = 0
aggrMin ms = minimum (map value ms)

aggregateSum :: [Metric] -> Double
aggregateSum = aggrSum

aggregateCount :: [Metric] -> Int
aggregateCount = aggrCount

aggregateAvg :: [Metric] -> Double
aggregateAvg = aggrAvg

aggregateMax :: [Metric] -> Double
aggregateMax = aggrMax

aggregateMin :: [Metric] -> Double
aggregateMin = aggrMin