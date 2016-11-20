{-# LANGUAGE Strict #-}
module SmallPT where
import MathTools
import Vector3
import Ray
import Sphere
import RNG
import Sampling
import ImageIO

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
intersect_scene scene ray = (intersect_scene_acc scene ray 0 (False, 0.0, 0))

intersect_scene_acc :: [Sphere] -> Ray -> Int -> (Bool, Double, Int) -> (Bool, Double, Int)
intersect_scene_acc [] _ _ acc = acc
intersect_scene_acc (head:tail) (Ray o d tmin tmax depth) index (chit, ctmax, csphere) = let (hit, smax) = (intersect_sphere head (Ray o d tmin tmax depth))
                                                                                             nindex = ((+) index 1)
                                                                                             in if hit
                                                                                                then intersect_scene_acc tail (Ray o d tmin smax depth) nindex (True, smax, index)
                                                                                                else intersect_scene_acc tail (Ray o d tmin tmax depth) nindex (chit, ctmax, csphere)

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
default_camera width height = let eye = (Vector3 50.0 52.0 295.6)
                                  gaze = (normalize_v3 (Vector3 0.0 (-0.042612) (-1.0)))
                                  fov = 0.5135
                                  cx = (Vector3 ((/) ((*) (realToFrac width) fov) (realToFrac height)) 0.0 0.0)
                                  cy = (mul_v3d (normalize_v3 (cross_v3v3 cx gaze)) fov)
                                  in (Camera eye cx cy gaze)

-- Main

loop_main :: [Sphere] -> Camera -> Int -> Int -> Int -> [Vector3]
loop_main scene cam height width smax = let ls = [] --(make_vector (* width height) (vector3 0.0 0.0 0.0))
                                            in (loop_y scene cam 0 height width smax ls)

loop_y :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> [Vector3] -> [Vector3]
loop_y scene cam y height width smax ls0 = if ((==) y height)
                                           then ls0
                                           else let ls1 = (loop_x scene cam y height 0 width smax ls0)
                                                    in (loop_y scene cam ((+) y 1) height width smax ls1)

 --      (write (format "Rendering (~v spp) ~v%~n" (* smax 4) (~r (* 100.0 (/ y (- height 1))) #:precision 2)))

loop_x :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> [Vector3] -> [Vector3]
loop_x scene cam y height x width smax ls0 = if ((==) x width)
                                             then ls0
                                             else let ls1 = (loop_sy scene cam y height x width 0 smax ls0)
                                                      in (loop_x scene cam y height ((+) x 1) width smax ls1)

loop_sy :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> Int -> [Vector3] -> [Vector3]
loop_sy scene cam y height x width sy smax ls0 = if ((==) sy 2)
                                                 then ls0
                                                 else let ls1 = (loop_sx scene cam y height x width sy 0 smax ls0)
                                                          in (loop_sy scene cam y height x width ((+) sy 1) smax ls1)

loop_sx :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Vector3] -> [Vector3]
loop_sx scene cam y height x width sy sx smax ls = if ((==) sx 2)
                                                   then ls
                                                   else let l0 = (loop_s scene cam y height x width sy sx 0 smax (Vector3 0.0 0.0 0.0))
                                                            l1 = (mul_v3d (clamp_v3 l0 0.0 1.0) 0.25)
                                                            index = ((+) ((*) ((-) ((-) height 1) y) width) x)
                                                            in (loop_sx scene cam y height x width sy ((+) sx 1) smax ls)
-- (vector-set! Ls index L1)
  
loop_s :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Vector3 -> Vector3
loop_s scene cam y height x width sy sx s smax l0 = if ((==) s smax)
                                                     then l0
                                                     else let l1 = (do_s scene cam y height x width sy sx smax l0)
                                                              in (loop_s scene cam y height x width sy sx ((+) s 1) smax l1)
do_s :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Vector3 -> Vector3
do_s scene cam y height x width sy sx smax l0 = Vector3 0.0 0.0 0.0