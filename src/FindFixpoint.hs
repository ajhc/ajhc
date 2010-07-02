module FindFixpoint(Ms, getVal, solve) where

import Array
import Control.Monad.Writer
import Data.Array.IO
import Data.Graph
import Data.IntSet as IntSet
import Util.Gen





data Env b  = Env {-# UNPACK #-} !(IOArray Int b) {-# UNPACK #-} !(IOArray Int (IntSet)) {-# UNPACK #-} !Int
newtype Ms b c = Ms' (Env b -> IO c)

instance Monad (Ms b) where
    return a = Ms' (\_ -> return a)
    Ms' comp >>= fun
        = Ms' (\v  -> comp v >>= \r -> case fun r   of Ms' x -> x v)
    Ms' a >> Ms' b = Ms' $ \v -> a v >> b v
    fail x = Ms' (\_ -> (putErrDie x))
    {-# INLINE (>>) #-}
    {-# INLINE (>>=) #-}
    {-# INLINE return #-}

instance Functor (Ms b) where
    fmap = liftM

unMs' (Ms' x) = x

{-# INLINE getVal #-}
getVal ::  Int -> Ms b b
getVal n = Ms' $ \(Env arr ref self) ->  do
    s <- readArray ref n
    writeArray ref n $ (IntSet.insert self s)
    readArray arr n



solve :: (Eq b) => Maybe String -> b -> [Ms b b] -> IO [b]
solve str' empty vs = do
    let put = case str' of
            Just _ -> putErrLn
            Nothing -> const (return ())
        put' = case str' of
            Just _ -> putErr
            Nothing -> const (return ())
        Just str = str'
    let len = length vs
    put $ "Finding Fixpoint for " ++ show len ++ " variables: " ++  str
    arr <- newArray (0,len - 1) empty
    ref <- newArray (0,len - 1) IntSet.empty
    let as = [ (i,(unMs' f) (Env arr ref i))  |  f <- vs | i <- [0..]]
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
            put' $ " " ++ show (IntSet.size s)
            li (reverse xs) IntSet.empty
        li (i:rs) s = do
            b <- readArray arr i
            b'<- fna Array.! i
            case b == b' of
                True -> li rs (IntSet.delete i s)
                False -> do
                    writeArray arr i b'
                    ns <- readArray ref i
                    li rs (ns `IntSet.union` IntSet.delete i s)
    li [0 .. len - 1] IntSet.empty
    put $ " Done."
    mapM (readArray arr)  [0 .. len - 1]


