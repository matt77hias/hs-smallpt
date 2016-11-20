{-# LANGUAGE Strict #-}
module Vector3 where
import MathTools(clamp)

data Vector3 = Vector3 Double Double Double

x_v3 :: Vector3 -> Double
x_v3 (Vector3 x _ _) = x

y_v3 :: Vector3 -> Double
y_v3 (Vector3 _ y _) = y

z_v3 :: Vector3 -> Double
z_v3 (Vector3 _ _ z) = z

get_v3 :: Vector3 -> Int -> Double
get_v3 (Vector3 x _ _) 0 = x
get_v3 (Vector3 _ y _) 1 = y
get_v3 (Vector3 _ _ z) 2 = z

minus_v3 :: Vector3 -> Vector3
minus_v3 (Vector3 x y z) = (Vector3 ((negate) x) ((negate) y) ((negate) z))

add_v3v3 :: Vector3 -> Vector3 -> Vector3 
add_v3v3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 ((+) x1 x2) ((+) y1 y2) ((+) z1 z2)

add_v3d :: Vector3 -> Double -> Vector3
add_v3d (Vector3 x y z) a = Vector3 ((+) x a) ((+) y a) ((+) z a)

add_dv3 :: Double -> Vector3 -> Vector3
add_dv3 a (Vector3 x y z) = Vector3 ((+) a x) ((+) a y) ((+) a z)

sub_v3v3 :: Vector3 -> Vector3 -> Vector3
sub_v3v3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 ((-) x1 x2) ((-) y1 y2) ((-) z1 z2)

sub_v3d :: Vector3 -> Double -> Vector3
sub_v3d (Vector3 x y z) a = Vector3 ((-) x a) ((-) y a) ((-) z a)

sub_dv3 :: Double -> Vector3 -> Vector3
sub_dv3 a (Vector3 x y z) = Vector3 ((-) a x) ((-) a y) ((-) a z)

mul_v3v3 :: Vector3 -> Vector3 -> Vector3
mul_v3v3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 ((*) x1 x2) ((*) y1 y2) ((*) z1 z2)

mul_v3d :: Vector3 -> Double -> Vector3
mul_v3d (Vector3 x y z) a = Vector3 ((*) x a) ((*) y a) ((*) z a)

mul_dv3 :: Double -> Vector3 -> Vector3
mul_dv3 a (Vector3 x y z) = Vector3 ((*) a x) ((*) a y) ((*) a z)

div_v3v3 :: Vector3 -> Vector3 -> Vector3
div_v3v3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 ((/) x1 x2) ((/) y1 y2) ((/) z1 z2)

div_v3d :: Vector3 -> Double -> Vector3
div_v3d (Vector3 x y z) a = Vector3 ((/) x a) ((/) y a) ((/) z a)

div_dv3 :: Double -> Vector3 -> Vector3
div_dv3 a (Vector3 x y z) = Vector3 ((/) a x) ((/) a y) ((/) a z)

dot_v3v3 :: Vector3 -> Vector3 -> Double
dot_v3v3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = ((+) ((*) x1 x2) ((+) ((*) y1 y2) ((*) z1 z2)))

cross_v3v3 :: Vector3 -> Vector3 -> Vector3
cross_v3v3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 ((-) ((*) y1 z2) ((*) z1 y2)) ((-) ((*) z1 x2) ((*) x1 z2)) ((-) ((*) x1 y2) ((*) y1 x2))

eq_v3v3 :: Vector3 -> Vector3 -> Bool
eq_v3v3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = ((&&) ((==) x1 x2) ((&&) ((==) y1 y2) ((==) z1 z2)))

ne_v3v3 :: Vector3 -> Vector3 -> Bool
ne_v3v3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = ((||) ((/=) x1 x2) ((||) ((/=) y1 y2) ((/=) z1 z2)))

le_v3v3 :: Vector3 -> Vector3 -> Bool
le_v3v3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = ((&&) ((<=) x1 x2) ((&&) ((<=) y1 y2) ((<=) z1 z2)))

lt_v3v3 :: Vector3 -> Vector3 -> Bool
lt_v3v3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = ((&&) ((<) x1 x2) ((&&) ((<) y1 y2) ((<) z1 z2)))

ge_v3v3 :: Vector3 -> Vector3 -> Bool
ge_v3v3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = ((&&) ((>=) x1 x2) ((&&) ((>=) y1 y2) ((>=) z1 z2)))

gt_v3v3 :: Vector3 -> Vector3 -> Bool
gt_v3v3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = ((&&) ((>) x1 x2) ((&&) ((>) y1 y2) ((>) z1 z2)))

mind_v3 :: Vector3 -> Int
mind_v3 (Vector3 x y z) = case ((&&) ((>) x y) ((>) x z)) of
                                True -> 1
                                _    -> case ((>) y z) of
                                            True -> 2
                                            _    -> 2

maxd_v3 :: Vector3 -> Int
maxd_v3 (Vector3 x y z) = case ((&&) ((<) x y) ((<) x z)) of
                                True -> 1
                                _    -> case ((<) y z) of
                                            True -> 2
                                            _    -> 2

min_v3 :: Vector3 -> Double
min_v3 (Vector3 x y z) = (min (min x y) z)

max_v3 :: Vector3 -> Double
max_v3 (Vector3 x y z) = (max (max x y) z)

apply_unary :: (Double -> Double) -> Vector3 -> Vector3
apply_unary f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

apply_binary :: (Double -> Double -> Double) -> Vector3 -> Vector3 -> Vector3
apply_binary f (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (f x1 x2) (f y1 y2) (f z1 z2)

sqrt_v3 :: Vector3 -> Vector3
sqrt_v3 v = (apply_unary (sqrt) v)

pow_v3 :: Vector3 -> Double -> Vector3
pow_v3 v e = (apply_unary (\x -> ((**) x e)) v)

abs_v3 :: Vector3 -> Vector3
abs_v3 v = (apply_unary (abs) v)

min_v3v3 :: Vector3 -> Vector3 -> Vector3
min_v3v3 v1 v2 = (apply_binary (min) v1 v2)

max_v3v3 :: Vector3 -> Vector3 -> Vector3
max_v3v3 v1 v2 = (apply_binary (max) v1 v2)

round_v3 :: Vector3 -> Vector3
round_v3 v = (apply_unary (\x -> (fromIntegral (round x))) v)

floor_v3 :: Vector3 -> Vector3
floor_v3 v = (apply_unary (\x -> (fromIntegral (floor x))) v)

ceil_v3 :: Vector3 -> Vector3
ceil_v3 v = (apply_unary (\x -> (fromIntegral (ceiling x))) v)

trunc_v3 :: Vector3 -> Vector3
trunc_v3 v = (apply_unary (\x -> (fromIntegral (truncate x))) v)

clamp_v3 :: Vector3 -> Double -> Double -> Vector3
clamp_v3 v low high = (apply_unary (\x -> (clamp x low high)) v)

lerp_v3v3 :: Double -> Vector3 -> Vector3 -> Vector3
lerp_v3v3 a v1 v2 = (add_v3v3 v2 (mul_v3d (sub_v3v3 v2 v1) a))

permute_v3 :: Vector3 -> Int -> Int -> Int -> Vector3
permute_v3 v x y z = Vector3 (get_v3 v x) (get_v3 v y) (get_v3 v z)

norm2s_v3 :: Vector3 -> Double
norm2s_v3 (Vector3 x y z) = ((+) ((*) x x) ((+) ((*) y y) ((*) z z)))

norm2_v3 :: Vector3 -> Double
norm2_v3 v = (sqrt (norm2s_v3 v))

normalize_v3 :: Vector3 -> Vector3
normalize_v3 v = (div_v3d v (norm2_v3 v))