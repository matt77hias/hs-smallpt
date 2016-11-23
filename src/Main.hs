{-# LANGUAGE Strict #-}
module Main where

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Mutable as M
import System.Environment(getArgs)
import Text.Printf(printf)

import MathTools
import Vector3
import Ray
import Sphere
import RNG
import Sampling
import ImageIO

import Specular

-- Scene
refractive_index_out :: Double
refractive_index_out = 1.0

refractive_index_in :: Double
refractive_index_in = 1.5

default_scene :: [Sphere]
default_scene = [(Sphere 1.0e5 (Vector3 100001.0 40.8 81.6)   (Vector3 0.0 0.0 0.0)    (Vector3 0.75 0.25 0.25)    Diffuse)
                ,(Sphere 1.0e5 (Vector3 (-99901.0) 40.8 81.6) (Vector3 0.0 0.0 0.0)    (Vector3 0.25 0.25 0.75)    Diffuse)
                ,(Sphere 1.0e5 (Vector3 50.0 40.8 1.0e5)      (Vector3 0.0 0.0 0.0)    (Vector3 0.75 0.75 0.75)    Diffuse)
                ,(Sphere 1.0e5 (Vector3 50.0 40.8 (-99830))   (Vector3 0.0 0.0 0.0)    (Vector3 0.0 0.0 0.0)       Diffuse)
                ,(Sphere 1.0e5 (Vector3 50.0 1.0e5 81.6)      (Vector3 0.0 0.0 0.0)    (Vector3 0.75 0.75 0.75)    Diffuse)
                ,(Sphere 1.0e5 (Vector3 50.0 (-99918.4) 81.6) (Vector3 0.0 0.0 0.0)    (Vector3 0.75 0.75 0.75)    Diffuse)
                ,(Sphere 16.5  (Vector3 27.0 16.5 47.0)       (Vector3 0.0 0.0 0.0)    (Vector3 0.999 0.999 0.999) Specular)
                ,(Sphere 16.5  (Vector3 73.0 16.5 78.0)       (Vector3 0.0 0.0 0.0)    (Vector3 0.999 0.999 0.999) Refractive)
                ,(Sphere 600.0 (Vector3 50.0 681.33 81.6)     (Vector3 12.0 12.0 12.0) (Vector3 0.0 0.0 0.0)       Diffuse)]

-- Scene intersect
intersect_scene :: [Sphere] -> Ray -> (Bool, Double, Int)
intersect_scene scene ray =
    intersect_scene_acc scene ray 0 (False, positive_infinity, 0)

intersect_scene_acc :: [Sphere] -> Ray -> Int -> (Bool, Double, Int) -> (Bool, Double, Int)
intersect_scene_acc [] _ _ acc =
   acc
intersect_scene_acc (head:tail) (Ray o d tmin tmax depth) index (chit, ctmax, csphere) =
    let (hit, smax) = intersect_sphere head (Ray o d tmin tmax depth)
        nindex = index + 1
        in if hit
            then intersect_scene_acc tail (Ray o d tmin smax depth) nindex (True, smax, index)
            else intersect_scene_acc tail (Ray o d tmin tmax depth) nindex (chit, ctmax, csphere)

-- Radiance
radiance :: [Sphere] -> Ray -> IO Vector3
radiance scene ray =
    let (hit, thit, index) = intersect_scene scene ray
        in if (not hit)
            then return (Vector3 0.0 0.0 0.0)
            else do 
                    let p = eval_ray ray thit
                        s = scene !! index
                        n = normalize_v3 $ sub_v3v3 p (get_sphere_p s)
                    (quit, nf) <- if ((get_ray_depth ray) > 4)
                                    then do
                                            let prC = max_v3 $ get_sphere_f s
                                            random <- uniform_float
                                            if (random >= prC)
                                                then return (True, Vector3 0.0 0.0 0.0)
                                                else return (False, (div_v3d (get_sphere_f s) prC))
                                    else return (False, (get_sphere_f s))
                    if (quit)
                        then return $ get_sphere_e s
                        else do 
                                (d, c) <- case (get_sphere_reflection_type s) of
                                            Refractive -> ideal_specular_transmit (get_ray_d ray) n refractive_index_out refractive_index_in
                                            Specular   -> return ((ideal_specular_reflect (get_ray_d ray) n), 1.0)
                                            _          -> do
                                                            let w = if ((dot_v3v3 n (get_ray_d ray)) < 0.0) then n else minus_v3 n
                                                                u1 = if ((abs $ x_v3 w) > 0.1) then (Vector3 0.0 1.0 0.0) else (Vector3 1.0 0.0 0.0)
                                                                u = normalize_v3 $ cross_v3v3 u1 w
                                                                v = cross_v3v3 w u
                                                            random1 <- uniform_float
                                                            random2 <- uniform_float
                                                            let dd = cosine_weighted_sample_on_hemisphere random1 random2
                                                            return (normalize_v3 $ (add_v3v3 (mul_dv3 (z_v3 dd) w) (add_v3v3 (mul_dv3 (x_v3 dd) u) (mul_dv3 (y_v3 dd) v))), 1.0)
                                let f = mul_v3d nf c
                                l <- radiance scene (Ray p d epsilon_sphere positive_infinity ((get_ray_depth ray) + 1))
                                return (add_v3v3 (get_sphere_e s) (mul_v3v3 f l))

-- Camera
data Camera = Camera Vector3 Vector3 Vector3 Vector3

get_camera_eye :: Camera -> Vector3
get_camera_eye (Camera eye _ _ _) = eye

get_camera_cx :: Camera -> Vector3
get_camera_cx (Camera _ cx _ _) = cx

get_camera_cy :: Camera -> Vector3
get_camera_cy (Camera _ _ cy _) = cy

get_camera_gaze :: Camera -> Vector3
get_camera_gaze (Camera _ _ _ gaze) = gaze

default_camera :: Int -> Int -> Camera
default_camera width height =
    let eye = Vector3 50.0 52.0 295.6
        gaze = normalize_v3 (Vector3 0.0 (-0.042612) (-1.0))
        fov = 0.5135
        cx = Vector3 ((realToFrac width) * fov / (realToFrac height)) 0.0 0.0
        cy = mul_v3d (normalize_v3 (cross_v3v3 cx gaze)) fov
        in Camera eye cx cy gaze

-- Main

main :: IO ()
main =
    do
        seed_rng 606418532
        smax <- get_nb_samples
        let width = 1024
            height = 768
            cam = default_camera width height
            scene = default_scene
        ls <- loop_main scene cam height width smax
        write_ppm_default width height ls

get_nb_samples :: IO Int
get_nb_samples =
    do
        args <- getArgs
        let l = length args
        if (l == 0)
            then return 1
            else let first = read (args !! 0) :: Int
                     in return $ quot first 4

loop_main :: [Sphere] -> Camera -> Int -> Int -> Int -> IO (M.MVector (PrimState IO) Vector3)
loop_main scene cam height width smax =
    do
        ls <- M.replicate (width * height) (Vector3 0.0 0.0 0.0)
        loop_y scene cam 0 height width smax ls
        return ls

loop_y :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> (M.MVector (PrimState IO) Vector3) -> IO ()
loop_y scene cam y height width smax ls =
    if (y == height)
        then return ()
        else do 
                putStr $ printf "Rendering (%d spp) %.2f\r" (smax * 4) ((100.0 * (realToFrac y) / (realToFrac (height - 1))) :: Float)
                loop_x scene cam y height 0 width smax ls
                loop_y scene cam (y + 1) height width smax ls

loop_x :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> (M.MVector (PrimState IO) Vector3) -> IO ()
loop_x scene cam y height x width smax ls =
    if (x == width)
        then return ()
        else do
                loop_sy scene cam y height x width 0 smax ls
                loop_x scene cam y height (x + 1) width smax ls

loop_sy :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> Int -> (M.MVector (PrimState IO) Vector3) -> IO ()
loop_sy scene cam y height x width sy smax ls =
    if (sy == 2)
        then return ()
        else do 
                loop_sx scene cam y height x width sy 0 smax ls
                loop_sy scene cam y height x width (sy + 1) smax ls

loop_sx :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (M.MVector (PrimState IO) Vector3) -> IO ()
loop_sx scene cam y height x width sy sx smax ls =
    if (sx == 2)
        then return ()
        else do
                l0 <- loop_s scene cam y height x width sy sx 0 smax (Vector3 0.0 0.0 0.0)
                let index = (height - 1 - y) * width + x
                    l1 = mul_v3d (clamp_v3 l0 0.0 1.0) 0.25
                l2 <- M.read ls index
                M.write ls index (add_v3v3 l2 l1)
                loop_sx scene cam y height x width sy (sx + 1) smax ls

loop_s :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Vector3 -> IO Vector3
loop_s scene cam y height x width sy sx s smax l0 =
    if (s == smax)
        then return l0
        else do
                l1 <- do_s scene cam y height x width sy sx smax l0
                loop_s scene cam y height x width sy sx (s + 1) smax l1

do_s :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Vector3 -> IO Vector3
do_s scene cam y height x width sy sx smax l0 =
    do 
        let df = \x -> let u = (2.0 * x) in if (u < 1.0) then (sqrt u) - 1.0 else 1.0 - (sqrt (2.0 - u))
        random1 <- uniform_float
        random2 <- uniform_float
        let coefx = (((((realToFrac sx) + 0.5 + (df random1)) / 2.0) + (realToFrac x)) / (realToFrac width)) - 0.5
            coefy = (((((realToFrac sy) + 0.5 + (df random2)) / 2.0) + (realToFrac y)) / (realToFrac height)) - 0.5
            direction = add_v3v3 (add_v3v3 (mul_v3d (get_camera_cx cam) coefx) (mul_v3d (get_camera_cy cam) coefy)) (get_camera_gaze cam)
            ndirection = normalize_v3 direction
            neye = add_v3v3 (get_camera_eye cam) (mul_v3d direction 130.0)
        l1 <- radiance scene (Ray neye ndirection epsilon_sphere positive_infinity 0)
        return $ add_v3v3 l0 (div_v3d l1 (realToFrac smax))