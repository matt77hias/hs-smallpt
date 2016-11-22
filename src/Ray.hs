{-# LANGUAGE Strict #-}
module Ray where
import Vector3(Vector3(Vector3), add_v3v3, mul_v3d)

data Ray = Ray Vector3 Vector3 Double Double Int

get_ray_o :: Ray -> Vector3
get_ray_o (Ray o _ _ _ _) = o

get_ray_d :: Ray -> Vector3
get_ray_d (Ray _ d _ _ _) = d

get_ray_tmin :: Ray -> Double
get_ray_tmin (Ray _ _ tmin _ _) = tmin

get_ray_tmax :: Ray -> Double
get_ray_tmax (Ray _ _ _ tmax _) = tmax

get_ray_depth :: Ray -> Int
get_ray_depth (Ray _ _ _ _ depth) = depth

eval_ray :: Ray -> Double -> Vector3
eval_ray (Ray o d _ _ _) t = add_v3v3 o (mul_v3d d t)