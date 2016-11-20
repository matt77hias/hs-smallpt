{-# LANGUAGE Strict #-}
module RNG where
import System.Random(setStdGen, mkStdGen, randomRIO)

seed_rng :: Int -> IO()
seed_rng seed = (setStdGen (mkStdGen seed))

uniform_float :: IO Double
uniform_float = (randomRIO (0.0, 1.0))