{-# OPTIONS_GHC -fglasgow-exts #-}
-- find fixpoint of constraint problem

module Fixer(
    Fixable(..),
    Value,
    newValue,
    findFixpoint,
    readValue,
    isSuperSetOf,
    value,
    modifiedSuperSetOf,
    newFixer,
    conditionalRule,
    dynamicRule
    ) where

import Data.IORef
import Monad
import IO(hFlush, stdout)


class Show a => Fixable a where
    bottom :: a
    isBottom :: a -> Bool
    lub :: a -> a -> a
    minus :: a -> a -> a

data MkFixable = forall a . Fixable a => MkFixable (RvValue a)
type Rules = IO ()
newtype Fixer  = Fixer { vars :: IORef [MkFixable] }

newFixer :: IO Fixer
newFixer = do
    v <- newIORef []
    return Fixer { vars = v }

data Value a = ConstValue a | IV (RvValue a)


data RvValue a = RvValue {
    action :: IORef [a -> IO ()],
    pending :: IORef a,
    current :: IORef a
    }


value :: a -> Value a
value x = ConstValue x

newValue :: Fixable a => Fixer -> a -> IO (Value a)
newValue Fixer { vars = vars } v = do
    pending <- newIORef bottom
    current <- newIORef bottom
    action <- newIORef []
    let value =  IV rv
        rv =  RvValue { current = current, pending = pending, action = action }
    modifyIORef vars (MkFixable rv:)
    propegateValue v value
    return value


addAction :: Fixable a => Value a -> (a -> IO ())  -> IO ()
addAction (ConstValue n) act = act n
addAction (IV v) act = do
    modifyIORef (action v) (act:)
    c <- readIORef (current v)
    unless (isBottom c) (act c)



-- | the function must satisfy the rule that if a >= b then f(a) >= f(b)

modifiedSuperSetOf :: (Fixable a, Fixable b) =>  Value b -> Value a -> (a -> b) -> IO ()
modifiedSuperSetOf v1 (ConstValue cv) r = propegateValue (r cv) v1
modifiedSuperSetOf v1 v2 r = addAction v2 (\x -> propegateValue (r x) v1)

isSuperSetOf :: Fixable a => Value a -> Value a -> IO ()
v1 `isSuperSetOf` (ConstValue v2) = propegateValue v2 v1
v1 `isSuperSetOf` v2 = addAction v2 (\x -> propegateValue x v1)

-- | the function must satisfy the rule that if a >= b then f(a) implies f(b)
conditionalRule :: Fixable a => (a -> Bool) -> Value a -> Rules -> IO ()
conditionalRule cond v act = addAction v (\x -> if cond x then act else return ())

dynamicRule  :: Fixable a =>  Value a -> (a -> Rules) -> IO ()
dynamicRule v dr = addAction v dr

propegateValue :: Fixable a => a -> Value a -> IO ()
propegateValue a _ | isBottom a = return ()
propegateValue _ (ConstValue _) = fail "Fixer: You cannot modify a constant value"
propegateValue p (IV v) = do
    modifyIORef (pending v) (lub p)
    {-
    c <- readIORef (current v)
    let diff = p `minus` c
    if isBottom diff then return () else do
    as <- readIORef (action v)
    writeIORef (current v) (p `lub` c)
    --writeIORef (pending v) bottom
    mapM_ ($ diff) as
    --f vs True
    -}


readValue :: Value a -> IO a
readValue (IV v) = readIORef (current v)
readValue (ConstValue v) = return v


findFixpoint :: Fixer -> IO ()
findFixpoint Fixer { vars = vars } = do
    vars <- readIORef vars
    putStrLn $ "Fixer: " ++ show (length vars)
    let f [] n | n > 0 = putStr "(" >> putStr (show n) >> putStr ")" >> hFlush stdout >> f vars 0
        f [] _ = putStrLn "" >> return ()
        f (MkFixable v:vs) n = do
            p <- readIORef (pending v)
            c <- readIORef (current v)
            let diff = p `minus` c
            if isBottom diff then f vs n else do
            as <- readIORef (action v)
            writeIORef (current v) (p `lub` c)
            writeIORef (pending v) bottom
            --putStr "["
            --putStr (show diff)
            --putStr "]"
            mapM_ ($ diff) as
            f vs $! (n + 1)
    f vars (0::Int)





