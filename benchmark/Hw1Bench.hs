module Hw1Bench (benchmarks) where

import Hw1.Hanoi (hanoi, hanoiAcc)
import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer (printAwkExpr)

benchmarks :: Benchmark
benchmarks = bgroup "Hw1" [hanoiBenchmarks]

hanoiBenchmarks :: Benchmark
hanoiBenchmarks = bgroup "hanoi" [hanoiAt n | n <- [10, 15, 20, 25]]

hanoiAt :: Integer -> Benchmark
hanoiAt n =
  bgroup
    (show n)
    [ bench "original" $ nf (\k -> hanoi k "a" "b" "c") n,
      bcompare
        (printAwkExpr $ locateBenchmark ["original", show n])
        (bench "accumulator" $ nf (\k -> hanoiAcc k "a" "b" "c") n)
    ]
