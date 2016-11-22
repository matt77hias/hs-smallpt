{-# LANGUAGE Strict #-}
module Sphere where
import Vector3(Vector3(Vector3), sub_v3v3, dot_v3v3)
import Ray(Ray(Ray))

epsilon_sphere :: Double
epsilon_sphere = 1.0e-4

data ReflectionType = Diffuse | Specular | Refractive

data Sphere = Sphere Double Vector3 Vector3 Vector3 ReflectionType

get_sphere_r :: Sphere -> Double
get_sphere_r (Sphere r _ _ _ _) = r

get_sphere_p :: Sphere -> Vector3
get_sphere_p (Sphere _ p _ _ _) = p

get_sphere_e :: Sphere -> Vector3
get_sphere_e (Sphere _ _ e _ _) = e

get_sphere_f :: Sphere -> Vector3
get_sphere_f (Sphere _ _ _ f _) = f

get_sphere_reflection_type :: Sphere -> ReflectionType
get_sphere_reflection_type (Sphere _ _ _ _ rt) = rt

intersect_sphere :: Sphere -> Ray -> (Bool, Double)
intersect_sphere (Sphere r p _ _ _) (Ray o d tmin tmax _) =
    let op = sub_v3v3 p o
        dop = dot_v3v3 d op
        discriminant = dop * dop - (dot_v3v3 op op) + (r * r)
        in if (discriminant < 0.0)
            then (False, 0.0)
            else let sdiscriminant = sqrt discriminant
                     smin = dop - sdiscriminant
                     in if ((tmin < smin) && (smin < tmax))
                            then (True, smin)
                            else let smax = dop + sdiscriminant
                                     in if ((tmin < smax) && (smax < tmax))
                                            then (True, smax)
                                            else (False, 0.0)