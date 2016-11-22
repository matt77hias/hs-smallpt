{-# LANGUAGE Strict #-}
module Specular where
import Vector3(Vector3(Vector3), minus_v3, sub_v3v3, mul_dv3, mul_v3d, dot_v3v3, normalize_v3)
import RNG(uniform_float)

reflectance0 :: Double -> Double -> Double
reflectance0 n1 n2 =
    let sr = (n1 - n2) / (n1 + n2)
        in sr * sr

schlick_reflectance :: Double -> Double -> Double -> Double
schlick_reflectance n1 n2 c =
    let r0 = reflectance0 n1 n2
        in r0 + (1 - r0) * c * c * c * c * c

ideal_specular_reflect :: Vector3 -> Vector3 -> Vector3
ideal_specular_reflect d n =
    sub_v3v3 d (mul_dv3 (2.0 * (dot_v3v3 n d)) n)

ideal_specular_transmit :: Vector3 -> Vector3 -> Double -> Double -> IO (Vector3, Double)
ideal_specular_transmit d n nout nin =
    let dRe = ideal_specular_reflect d n
        out_to_in = (dot_v3v3 n d) < 0.0
        nl = if out_to_in then n else minus_v3 n
        nn = if out_to_in then nout / nin else nin / nout
        cos_theta = dot_v3v3 d nl
        cos2_phi = 1.0 - nn * nn * (1.0 - cos_theta * cos_theta)
        in if (cos2_phi < 0.0)
            then return (dRe, 1.0)
            else do
                    let dTr = normalize_v3 $ sub_v3v3 (mul_dv3 nn d) (mul_v3d nl ((nn * cos_theta) + (sqrt cos2_phi)))
                        c = if out_to_in then 1.0 + cos_theta else 1.0 - (dot_v3v3 dTr n)
                        re = schlick_reflectance nout nin c
                        prRe = 0.25 + 0.5 * re
                    u <- uniform_float
                    if (u < prRe)
                        then return (dRe, (re / prRe))
                        else return (dTr, ((1.0 - re) / (1.0 - prRe)))