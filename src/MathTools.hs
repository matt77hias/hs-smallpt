{-# LANGUAGE Strict #-}
module MathTools where
import Prelude hiding (pi)

pi :: Double
pi = 3.14159265358979323846

positive_infinity :: Double
positive_infinity = 1.0e20

clamp :: Double -> Double -> Double -> Double
clamp x low high = 
    if (x < low) then low else (if (x > high) then high else x)

to_byte :: Double -> Double -> Int
to_byte x gamma = 
    let e = 1.0 / gamma
        t = 255.0 * (x ** e)
        in truncate $ clamp t 0.0 255.0