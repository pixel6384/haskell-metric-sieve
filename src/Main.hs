{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Sieve

main :: IO ()
main = do
  args <- getArgs
  case args of
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
    _ -> putStrLn "Usage: metric-sieve <metric_name> <threshold>"