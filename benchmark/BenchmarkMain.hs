module Main (main) where

import qualified Hw1Bench
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main = defaultMain [Hw1Bench.benchmarks]
