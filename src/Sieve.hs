{-# LANGUAGE OverloadedStrings #-}
module Sieve (Metric(..), parseLine, filterMetric, aggregateMetrics) where

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

-- | Simple fold for calculating average
aggregateMetrics :: [Metric] -> Double
aggregateMetrics [] = 0
aggregateMetrics ms = sum (map value ms) / fromIntegral (length ms)