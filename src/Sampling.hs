{-# LANGUAGE Strict #-}
module Sampling where
import Prelude hiding (pi)
import MathTools(pi)
import Vector3(Vector3(Vector3))

uniform_sample_on_hemisphere :: Double -> Double -> Vector3
uniform_sample_on_hemisphere u1 u2 = let sin_theta = (sqrt (max 0.0 ((-) 1.0 ((*) u1 u1))))
                                         phi = ((*) 2.0 ((*) (pi) u2))
                                         in Vector3 ((*) (cos phi) sin_theta) ((*) (sin phi) sin_theta) u1

cosine_weighted_sample_on_hemisphere :: Double -> Double -> Vector3
cosine_weighted_sample_on_hemisphere u1 u2 = let cos_theta = (sqrt ((-) 1.0 u1))
                                                 sin_theta = (sqrt u1)
                                                 phi = ((*) 2.0 ((*) (pi) u2))
                                                 in Vector3 ((*) (cos phi) sin_theta) ((*) (sin phi) sin_theta) cos_theta