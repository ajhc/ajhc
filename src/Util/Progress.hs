module Util.Progress(
    Progress(),
    progressNew,
    progressStep,
    progressIOSteps,
    progressIONew,
    progressSteps
    )where


import System.IO
import Data.IORef


data Progress k = Progress {
    pTreap :: Treap k Double,
    pIncrement,pDecrement,pBias,pTotal :: !Double
    }


instance Show (Progress k) where
    showsPrec n pr = showsPrec n (toPercent $ pTotal pr) . showChar '%'


progressIONew
    :: Int
    -> Int
    -> Char
    -> IO (IORef (Progress Char))
progressIONew nSteps nOut dChar = do
    let (pr,is) = progressStep (progressNew (nSteps + 1) nOut) dChar
    hPutStr stderr is
    newIORef pr


progressIOSteps :: IORef (Progress Char) -> [Char] -> IO ()
progressIOSteps ref ks = do
    pr <- readIORef ref
    let (pr',os) = progressSteps pr ks
    hPutStr stderr os
    writeIORef ref pr'


progressNew
    :: Int  -- ^ number of steps
    -> Int  -- ^ number of output positions
    -> Progress k
progressNew nSteps nOut = Progress {
    pTreap = Nil,
    pBias = - 0.5 / fromIntegral nOut ,
    pTotal = 0,
    pIncrement = 1.0 / fromIntegral nSteps,
    pDecrement = 1.0 / fromIntegral nOut
    }

progressSteps :: Ord k => Progress k -> [k] -> (Progress k,[k])
progressSteps pr ks = foldr fn (pr,[]) ks where
    fn k (pr,ks) = (pr',ks' ++ ks) where
        (pr',ks') = progressStep pr k

progressStep :: Ord k => Progress k -> k -> (Progress k,[k])
progressStep pr k = (pr { pTreap = ot, pBias = nb, pTotal = pTotal pr + pIncrement pr },ks) where
    dec = pDecrement pr
    itreap = insertWith (+) k (negate $ pIncrement pr) (pTreap pr)
    (ot,nb,ks) = f (pBias pr - pIncrement pr) itreap []
    f b t ks | b <= negate dec = f (b + dec) (insertWith (+) k dec t) (k:ks)
             | otherwise = (t,b,ks) where
        Just (k,p,t') = extract t

toPercent :: Double -> Double
toPercent d = (/ 100) . fromIntegral $ round (d * 10000)

histogram :: Ord k => [k] -> (Int,[(k,Int)])
histogram ks = mapSnd toListByPriority (foldr f (0,Nil) ks) where
    f k (n,t) = (n - 1,insertWith (+) k (-1) t)
    mapSnd f (x,y) = (x,f y)

histogramP :: Ord k => [k] -> [(k,Double)]
histogramP ks = [ (x,toPercent $ fromIntegral y / t) | (x,y) <- hs] where
    (ti,hs) = histogram ks
    t = fromIntegral ti

p = progressNew 7 10
p2 = progressNew 12 7
s7 = "..xxw.."
s12 = ".x.y.xx...x."

toListByPriority :: Treap k p -> [(k,p)]
toListByPriority t = f t [] where
    f Nil rs = rs
    f (Fork k p t1 t2) rs = f t1 ((k,p):f t2 rs)



data Treap k p = Nil | Fork k p (Treap k p) (Treap k p)
    deriving(Show)

lookup :: Ord k => k -> Treap k p -> Maybe p
lookup k t = f t where
    f Nil = Nothing
    f (Fork k' p t1 t2) = case compare k k' of
        LT -> f t1
        GT -> f t2
        EQ -> Just p

merge :: Ord p => Treap k p -> Treap k p -> Treap k p
merge Nil t = t
merge t Nil = t
merge a@(Fork kx x x1 x2) b@(Fork ky y y1 y2)
    | x > y = Fork kx x x1 (merge x2 b)
    | otherwise = Fork ky y (merge a y1) y2

extract :: (Ord k,Ord p) => Treap k p -> Maybe (k,p,Treap k p)
extract Nil = Nothing
extract (Fork kx x t1 t2) = Just (kx,x,merge t1 t2)

fromList [] = Nil
fromList ((k,p):rs) = insert k p (fromList rs)

insertWith :: (Ord k,Ord p) => (p -> p -> p) -> k -> p -> Treap k p -> Treap k p
insertWith fp k p t = f t where
    f Nil = Fork k p Nil Nil
    f (Fork k' p' t1 t2) = case compare k k' of
        LT -> ins k' p' (f t1) t2
        GT -> ins k' p' t1 (f t2)
        EQ -> ins k (fp p p') t1 t2

    ins k p Nil Nil = Fork k p Nil Nil
    ins k p (Fork k' p' l r) t2 | p > p' = Fork k' p' l (ins k p r t2)
    ins k p t1 (Fork k' p' l r) | p > p' = Fork k' p' (ins k p t1 l) r
    ins k p t1 t2 = Fork k p t1 t2

insert :: (Ord k,Ord p) => k -> p -> Treap k p -> Treap k p
insert k p t = insertWith const k p t



-- very simple priority queue


{-
data Heap a = Nil | Fork a (Heap a) (Heap a)

isEmpty Nil = True
isEmpty _ = False

minElem (Fork x a b) = Just (x,merge a b)
minElem _ = Nothing

insert x a = merge (Fork x Nil Nil) a

merge a Nil = a
merge Nil b = b
merge a@(Fork x _ _) b@(Fork y _ _)
    | x <= y = join a b
    | otherwise = join b a
    where
    join (Fork x a b) c = Fork x b (merge a c)


discard :: Ord a => a -> Heap a -> Heap a
discard b h = f h where
    f Nil = Nil
    f (Fork x h1 h2)
        | x < b = merge (f h1) (f h2)
        | otherwise = merge h1 h2

-}
