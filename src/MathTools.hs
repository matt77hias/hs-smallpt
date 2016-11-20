{-# LANGUAGE Strict #-}
module MathTools where
import Prelude hiding (pi)

pi :: Double
pi = 3.14159265358979323846

positive_infinity :: Double
positive_infinity = 1.0e20

clamp :: Double -> Double -> Double -> Double
clamp x low high = case ((<) x low) of
                        True -> low
                        _    -> case ((>) x high) of
                                    True -> high
                                    _    -> x

to_byte :: Double -> Double -> Int
to_byte x gamma = let e = ((/) 1.0 gamma)
                      t = ((*) 255.0 ((**) x e))
                      in (truncate (clamp t 0.0 255.0))