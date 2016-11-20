{-# LANGUAGE Strict #-}
module ImageIO where
import System.IO(withFile, IOMode(WriteMode), Handle, hPutStr)
import Text.Printf(printf)
import MathTools(to_byte)
import Vector3(Vector3(Vector3))

gamma :: Double
gamma = 2.2

write_ppm_default :: Int -> Int -> [Vector3] -> IO ()
write_ppm_default width height ls = (write_ppm width height ls "hs-smallpt.ppm")

write_ppm :: Int -> Int -> [Vector3] -> String -> IO ()
write_ppm width height ls fname = withFile fname WriteMode $ \handle -> do
                                    let header = (printf "P3\n%d %d\n255\n" width height)
                                        in case ls of
                                           [] -> (hPutStr handle header)
                                           _  -> (seq (hPutStr handle header) (write_ppm_Ls handle ls))

write_ppm_Ls :: Handle -> [Vector3] -> IO ()
write_ppm_Ls handle (head:[]) = write_ppm_L handle head
write_ppm_Ls handle (head:tail) = (seq (seq (write_ppm_L handle head) (hPutStr handle " ")) (write_ppm_Ls handle tail))

write_ppm_L :: Handle -> Vector3 -> IO ()
write_ppm_L handle (Vector3 x y z) = hPutStr handle (printf "%d %d %d" (to_byte x (gamma)) (to_byte y (gamma)) (to_byte z (gamma)))