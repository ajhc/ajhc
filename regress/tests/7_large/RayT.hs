import System
infinity = 1/0
delta = sqrt e where e = encodeFloat (floatRadix e) (-floatDigits e)
infixl 7 .*, *|
data Vector = V !Double !Double !Double deriving (Show, Eq)
s *| V x y z = V (s * x) (s * y) (s * z)
instance Num Vector where
    V x y z + V x' y' z' = V (x + x') (y + y') (z + z')
    V x y z - V x' y' z' = V (x - x') (y - y') (z - z')
    fromInteger i = V x x x where x = fromInteger i
V x y z .* V x' y' z' = x * x' + y * y' + z * z'
vlength r = sqrt (r .* r)
unitise r = 1 / vlength r *| r

data Scene
    = Sphere !Vector !Double
    | Group !Vector !Double Scene Scene Scene Scene Scene
    deriving (Show)

ray_sphere (V dx dy dz) (V vx vy vz) r =
  let disc = vx * vx + vy * vy + vz * vz - r * r
  in  if disc < 0 then infinity else
      let b = vx * dx + vy * dy + vz * dz
          b2 = b * b
      in  if b2 < disc then infinity else
          let disk = sqrt(b2 - disc)
              t1 = b - disk
          in  if t1 > 0 then t1 else b + disk

ray_sphere' (V ox oy oz) (V dx dy dz) (V cx cy cz) r =
  let vx = cx - ox; vy = cy - oy; vz = cz - oz
      vv = vx * vx + vy * vy + vz * vz
      b = vx * dx + vy * dy + vz * dz
      disc = b * b - vv + r * r
  in  disc >= 0 && b + sqrt disc >= 0

data Hit = H {l :: !Double, nv :: Vector }

intersect dir@(V dx dy dz) hit s = case s of
    Sphere center@(V cx cy cz) radius ->
      let l' = ray_sphere dir center radius in
      if l' >= l hit then hit else
	let x = l' * dx - cx
	    y = l' * dy - cy
	    z = l' * dz - cz
	    il = 1 / sqrt(x * x + y * y + z * z)
	in  H {l = l', nv = V (il * x) (il * y) (il * z) }
    Group center radius a b c d e ->
      let l' = ray_sphere dir center radius in
      if l' >= l hit then hit else
	let f h s = intersect dir h s in
	f (f (f (f (f hit a) b) c) d) e

intersect' orig dir s = case s of
    Sphere center radius -> ray_sphere' orig dir center radius
    Group center radius a b c d e ->
      let f s = intersect' orig dir s in
      ray_sphere' orig dir center radius && (f a || f b || f c || f d || f e)

neg_light = unitise (V 1 3 (-2))

ray_trace dir scene =
  let hit = intersect dir (H infinity 0) scene in
  if l hit == infinity then 0 else
    let n = nv hit in
    let g = n .* neg_light in
    if g < 0 then 0 else
      if intersect' (l hit *| dir + delta *| n) neg_light scene then 0 else g

fold5 f x a b c d e = f (f (f (f (f x a) b) c) d) e

create level c r =
  let obj = Sphere c r in
  if level == 1 then obj else
    let a = 3 * r / sqrt 12 in
    let bound (c, r) s = case s of
	 Sphere c' r' -> (c, max r (vlength (c - c') + r'))
         Group _ _ v w x y z -> fold5 bound (c, r) v w x y z in
    let aux x' z' = create (level - 1 :: Int) (c + V x' a z') (0.5 * r) in
    let w = aux (-a) (-a); x = aux a (-a) in
    let y = aux (-a) a; z = aux a a in
    let (c1, r1) = fold5 bound (c + V 0 r 0, 0) obj w x y z in
    Group c1 r1 obj w x y z

ss = 4
pixel_vals n scene y x = sum
  [ let f a da = a - n / 2 + da / ss; d = unitise (V (f x dx) (f y dy) n)
    in  ray_trace d scene | dx <- [0..ss-1], dy <- [0..ss-1] ]
main = do 
    [level,ni] <- fmap (map read) getArgs
    let n = fromIntegral ni
	scene = create level (V 0 (-1) 4) 1  
	scale x = 0.5 + 255 * x / (ss*ss)
	picture = [ toEnum $ truncate $ scale $ pixel_vals n scene y x | y <- [n-1,n-2..0], x <- [0..n-1]]
    putStrLn $ "P5\n" ++ show ni ++ " " ++ show ni ++ "\n255\n" ++ picture
