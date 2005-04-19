module FixpointFinder(Ms, Ms', getVal', solve', getVal, solve) where
-- This can be sped up by taking dependencies into account.
-- I designed the interface so hopefully changes can be limited to
-- this module.

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Map as Map hiding((!),map) 
import Seq
import Data.Set as Set
import Data.IntSet as IntSet
import Data.Graph(stronglyConnComp, SCC(..),flattenSCCs)
import DDataUtil
import GenUtil
import Monad(liftM)
import Data.Array.IO
import System.IO.Unsafe
import CharIO
import Array
import GHC.Exts
import Data.IORef


{-
data FFEnv a b = FFEnv { 
        ffarray :: IOArray Int (b,[Int]), 
        ffmap :: Map a Int
    }

newtype  Ms a b c = Ms (FFEnv a b -> WriterT (Seq Int) IO c)


instance Monad (Ms a b) where
    return a = Ms (\_ -> return a) 
    Ms comp >>= fun
        = Ms (\v  -> comp v >>= \r -> case fun r   of Ms x -> x v) 
    Ms a >> Ms b = Ms $ \v -> a v >> b v
    fail x = Ms (\_ -> lift (CharIO.putErrDie x))


instance Functor (Ms a b) where
    fmap = liftM

getVal :: (Eq b,Ord a,Show a) => a -> Ms a b b
getVal x = Ms $ \ff -> do
    --let ind = unsafePerformIO (putErrLn $ "Lookup... " ++ show x) `seq` Map.find x (ffmap ff) 
    let ind =  Map.find x (ffmap ff) 
    tell (Seq.single ind)
    (n,_) <- lift $ readArray (ffarray ff) ind 
    return n

solve :: (Show a,Show b,Ord a, Eq b) => b -> [(a,Ms a b b)] -> IO [(a,b)]
solve _ [] = return []
solve empty vs = do 
    let len = length vs
    CharIO.putStrLn $ "solve: " ++ show len
    arr <- newArray (0::Int,len - 1) (empty,[])
    let aarr = listArray (0::Int,len -1) (fsts vs)
    let as =  [ (a,i,mb ff) | (a,Ms mb) <- vs | i <- [0..] ]
        mp = Map.fromList [ (a,i) | (a,i,_) <- as ]
        ff = FFEnv { ffarray = arr, ffmap = mp }
    let s (a,n,fn) = do
            (r,is) <- runWriterT fn 
            writeArray arr n (r,snub (Seq.toList is))
            --print (a,r)
            if (r /= empty) 
                then return [n]
                else return []
    cs <- fmap (Set.fromList . concat) $ mapM s as
    cs <- return $ Set.fromAscList [0..len - 1]
    CharIO.putStrLn $ "solve initial changed: " ++ show (Set.size cs) 
    let z (a,n,fn) = readArray arr n >>= \(_,ds) -> return ((a,n,fn),ds) 
    rs <- mapM z as 
    let as' = concatMap f $ stronglyConnComp [ ((a,n,fn),n,ds) | ((a,n,fn),ds) <- rs ] where
                f (AcyclicSCC x) = [x]
                f (CyclicSCC xs) = xs
        (_,sn,_) = head as'
    let f _ cs | Set.isEmpty cs = return ()
        f ((a,n,fn):rs) cs = do
            when (n == sn) $ do
                CharIO.putStrLn $ "Iteration... " ++ (show $ Set.size cs) 
                --mapM_ (\n ->  readArray arr n >>= \(x,_) ->  CharIO.print (aarr!n,x))  (Set.toList cs)    
            (v,ls) <- readArray arr n 
            if any (`Set.member` cs) ls  then do
                (r,is) <- runWriterT fn 
                if r /= v then 
                    writeArray arr n (r,snub (Seq.toList is)) >> 
                        --print (a,r) >> 
                           f rs (cs `Set.union` Set.single n)  
                 else f rs (cs Set.\\  Set.single n)
              else  f rs (cs Set.\\  Set.single n)
                
    f (cycle as') cs 
    let g (a,n,_) = do
            (v,_) <- readArray arr n 
            return  (a,v)
    mapM g as 
    
-}
{-
    return $ g vs (Map.fromList [ (a,empty) | (a,_) <- vs ]) [] where
        
    g ((a,Ms mb):vs) m rs = g vs m (r:rs) where
        r = (a,Ms mb, Seq.toList ds, b')
        (b',ds) = runWriter (mb m)  
        
    g [] _ rs | z `seq` True = f xs nm [] where  
        xs = concatMap f $ stronglyConnComp [ ((a,ds,m),a,ds) | (a,m,ds@(_:_),_) <- rs ] where
                f (AcyclicSCC x) = [x]
                f (CyclicSCC xs) = xs
        nm = Map.fromList [ (a,b) | (a,_,_,b) <- rs]
        z = unsafePerformIO  $ mapM print [ (a,ds) | (a,ds,_) <- xs ]
        
        f ((a,_,Ms mb):vs) m t = f vs m' (if  not theSame then (a,b'):t else t) where
            theSame = b == b'
            m' = if theSame then m else Map.insert a b' m
            Just b = Map.lookup a m
            (b',_) = runWriter (mb m)  
        f [] m [] = Map.toList m
        f [] m zs | unsafePerformIO (mapM_ print (fsts zs) >> print ([length zs]) ) `seq` True = f xs m []

-}

--data unboxed Mode = Waiting | InProgress | RecursiveLoop 

newtype (Ord a, Eq b) => Ms a b c = Ms (WriterT (Seq a) ((->) (Map a b))  c)
    deriving (Monad,Functor)

intNubRev :: [Int] -> [Int]
intNubRev xs = f xs [] IntSet.empty where
    f [] ys _ = ys 
    f (x:xs) ys m 
        | x `IntSet.member` m = f xs ys m 
        | otherwise = f xs (x:ys) (IntSet.insert x m)

getVal :: (Eq b,Ord a) => a -> Ms a b b
getVal x = Ms $ do
    tell (Seq.single x)
    m <- ask
    Just v <- return $ Map.lookup x m 
    return v

solve :: (Show a,Show b,Ord a, Eq b) => b -> [(a,Ms a b b)] -> IO [(a,b)]
solve empty vs = return $ g vs (Map.fromList [ (a,empty) | (a,_) <- vs ]) [] where
    g ((a,Ms mb):vs) m rs = g vs m (r:rs) where
        r = (a,Ms mb, Seq.toList ds, b')
        (b',ds) = runWriterT mb m  
    g [] _ rs  = f xs nm [] where  
        xs = concatMap f $ stronglyConnComp [ ((a,ds,m),a,ds) | (a,m,ds@(_:_),_) <- rs ] where
                f (AcyclicSCC x) = [x]
                f (CyclicSCC xs) = xs
        nm = Map.fromList [ (a,b) | (a,_,_,b) <- rs]
        
        f ((a,_,Ms mb):vs) m t = f vs m' (if  not theSame then (a,b'):t else t) where
            theSame = b == b'
            m' = if theSame then m else Map.insert a b' m
            Just b = Map.lookup a m
            (b',_) = runWriterT mb m  
        f [] m [] = Map.toList m
        f [] m zs | unsafePerformIO (mapM_ CharIO.print (if length zs < 10 then  zs else []) >> CharIO.print ([length zs]) ) `seq` True = f xs m []


data Env b  = Env {-# UNPACK #-} !(IOArray Int b) {-# UNPACK #-} !(IOArray Int (IntSet)) !Int
newtype Ms' b c = Ms' (IO c)
    deriving(Monad,Functor)

{-
instance Monad (Ms' b) where                                            
    return a = Ms' (\_ -> return a) 
    Ms' comp >>= fun
        = Ms' (\v  -> comp v >>= \r -> case fun r   of Ms' x -> x v) 
    Ms' a >> Ms' b = Ms' $ \v -> a v >> b v
    fail x = Ms' (\_ -> (CharIO.putErrDie x))
    {-# INLINE (>>) #-}  
    {-# INLINE (>>=) #-}  
    {-# INLINE return #-}  

instance Functor (Ms' b) where
    fmap = liftM
 -}

unMs' (Ms' x) = x

{-# INLINE getVal' #-}  
getVal' :: Env b -> Int -> Ms' b b 
getVal' (Env arr ref self) n = Ms' $  do
    s <- readArray ref n 
    --unless (self `IntSet.member` s) 
    writeArray ref n $ (IntSet.insert self s)
    readArray arr n 
    
    

solve' :: (Eq b) => b -> [Env b -> Ms' b b] -> IO [b]
solve' (empty :: b) vs = do
    CharIO.putErrLn $ "Solver: " ++ show (length vs)
    let len = length vs
    arr <- newArray (0,len - 1) empty
    ref <- newArray (0,len - 1) IntSet.empty
    let as = [ (i,unMs' $! f (Env arr ref i))  |  f <- vs | i <- [0..]]
        fna = listArray (0,len - 1) (snds as) 
    let li [] s | IntSet.null s  = return ()
        --li xs [] n = CharIO.putErr ("[" ++ show (I# n) ++ "]") >>   li xs xs 0#
        li [] s = do
            let g i = do
                    ds <- readArray ref i 
                    return (i,i,IntSet.toList ds)
            ds <- mapM g (IntSet.toList s)
            let xs = flattenSCCs scc
                scc =  stronglyConnComp ds
            CharIO.putErr $ " " ++ show (IntSet.size s)
            li (reverse xs) IntSet.empty
        li (i:rs) s = do
            b <- readArray arr i
            b'<- (fna Array.! i :: IO b)  
            case b == b' of
                True -> li rs (IntSet.delete i s) 
                False -> do
                    writeArray arr i b'
                    ns <- readArray ref i  
                    li rs (ns `IntSet.union` IntSet.delete i s) 
    li [0 .. len - 1] IntSet.empty
    CharIO.putErrLn $ " Done."
    mapM (readArray arr)  [0 .. len - 1]
-- The kind inference monad
    
{-
    let f as [] 0# _ _  = return ()  
        f as [] n ds l = do
            CharIO.putErrLn ("Iteration... " ++ show (I# n)) 
            let xs = concatMap z scc
                z (AcyclicSCC x) = [x]
                z (CyclicSCC xs) = xs
                scc =  stronglyConnComp ds
                g (AcyclicSCC (i,fn)) = do
                    b <- fn    
                    writeArray arr i b
                g (CyclicSCC xs) = CharIO.putErr "LI: " >> li xs xs 0# >> CharIO.putErrLn "" 
            writeIORef ref []
            mapM_ g scc  

            f xs xs 0# [] l
            --mapM CharIO.print (map (map fst . z) scc) 
            {-
            case l of 
                0# -> do
                    mapM_ g scc
                    f xs xs 0# [] (l +# 1#)
                5# -> f as as 0# [] 0#
                _ ->  f as as 0# [] (l +# 1#)
            -}
        f as ((i,fn):rs) n ds l = do
            b <- readArray arr i 
            writeIORef ref [-1]
            b' <- fn 
            nds <- readIORef ref 
            case b == b' of
                True -> f as rs n (((i,fn),i,nds):ds) l
                False -> do
                    writeArray arr i b'
                    f as rs (n +# 1#)  (((i,fn),i,nds):ds) l
        li _ [] 0# = return ()
        li xs [] n = CharIO.putErr ("(" ++ show (I# n) ++ ")") >>   li xs xs 0#
        li xs ((i,fn):rs) n = do
            b <- readArray arr i
            b' <- fn 
            case b == b' of
                True -> li xs rs n 
                False -> do
                    writeArray arr i b'
                    li xs rs (n +# 1#) 
    f as as 0# [] ()
    mapM (readArray arr)  [0 .. len - 1]
-- The kind inference monad

data KiEnv  = KiEnv {
    kiContext :: [String],
    kiEnv :: IORef KindEnv,
    kiSubst :: IORef Subst,
    kiVarnum :: IORef Int
    }

newtype KI a = KI (KiEnv -> IO a)-- -> (a, State))


instance Monad KI where
    return a = KI (\_ -> return a) 
    KI comp >>= fun
        = KI (\v  -> comp v >>= \r -> case fun r   of KI x -> x v) 
    fail x = KI (\s -> fail (unlines $ reverse (x:kiContext s)))

data State = State {
      env :: KindEnv,     -- the environment of kind assumptions 
      subst :: Subst     -- the current substitution
   }

-}
