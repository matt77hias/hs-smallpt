{-# LANGUAGE Strict #-}
module ImageIO where
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Mutable as M
import System.IO(withFile, IOMode(WriteMode), Handle, hPutStr)
import Text.Printf(printf)

import MathTools(to_byte)
import Vector3(Vector3(Vector3))

gamma :: Double
gamma = 2.2

write_ppm_default :: Int -> Int -> (M.MVector (PrimState IO) Vector3) -> IO ()
write_ppm_default width height ls =
    write_ppm width height ls "hs-smallpt.ppm"

write_ppm :: Int -> Int -> (M.MVector (PrimState IO) Vector3) -> String -> IO ()
write_ppm width height ls fname =
    withFile fname WriteMode $ \handle -> do
        let header = printf "P3\n%d %d\n255\n" width height
        hPutStr handle header
        write_ppm_Ls handle 0 (width * height) ls

write_ppm_Ls :: Handle -> Int -> Int -> (M.MVector (PrimState IO) Vector3) -> IO ()
write_ppm_Ls _ _ 0 _ = return ()
write_ppm_Ls handle index size ls =
    do
        _l <- M.read ls index
        if (index == size - 1)
            then do
                    write_ppm_L handle _l
            else do
                    write_ppm_L handle _l
                    hPutStr handle " "
                    write_ppm_Ls handle (index + 1) size ls

write_ppm_L :: Handle -> Vector3 -> IO ()
write_ppm_L handle (Vector3 x y z) =
    do
        hPutStr handle $ printf "%d %d %d" (to_byte x gamma) (to_byte y gamma) (to_byte z gamma)