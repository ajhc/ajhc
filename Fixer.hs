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
import Data.Typeable
import Data.Unique
import IO(hFlush, stdout)
import Monad
import qualified Data.Set as Set


class Fixable a where
    bottom :: a
    isBottom :: a -> Bool
    lub :: a -> a -> a
    minus :: a -> a -> a

data MkFixable = forall a . Fixable a => MkFixable (RvValue a)
type Rules = IO ()

data Fixer  = Fixer {
    vars :: !(IORef [MkFixable]),
    todo :: !(IORef (Set.Set MkFixable))
    }


newFixer :: IO Fixer
newFixer = do
    v <- newIORef []
    t <- newIORef Set.empty
    return Fixer { vars = v, todo = t }

data Value a = ConstValue a | IV (RvValue a)
    deriving(Typeable)

instance Show a => Show (Value a) where
    showsPrec _ (ConstValue a) = showString "<<" . shows a . showString ">>"
    showsPrec _ (IV a) = showString "<<" . shows (hashUnique $ ident a) . showString ">>"


data RvValue a = RvValue {
    ident :: {-# UNPACK #-} !Unique,
    action :: !(IORef [a -> IO ()]),
    pending :: !(IORef a),
    current :: !(IORef a),
    fixer :: Fixer
    }

instance Eq MkFixable where
    MkFixable a == MkFixable b = ident a == ident b
    MkFixable a /= MkFixable b = ident a /= ident b
instance Ord MkFixable where
    MkFixable a `compare` MkFixable b = ident a `compare` ident b
    MkFixable a >= MkFixable b = ident a >= ident b
    MkFixable a <= MkFixable b = ident a <= ident b
    MkFixable a > MkFixable b = ident a > ident b
    MkFixable a < MkFixable b = ident a < ident b

value :: a -> Value a
value x = ConstValue x

newValue :: Fixable a => Fixer -> a -> IO (Value a)
newValue fixer@Fixer { vars = vars } v = do
    ident <- newUnique
    pending <- newIORef bottom
    current <- newIORef bottom
    action <- newIORef []
    let value =  IV rv
        rv =  RvValue { ident = ident, fixer = fixer, current = current, pending = pending, action = action }
    modifyIORef vars (MkFixable rv:)
    propegateValue v rv
    return value


addAction :: Fixable a => Value a -> (a -> IO ())  -> IO ()
addAction (ConstValue n) act = act n
addAction (IV v) act = do
    modifyIORef (action v) (act:)
    c <- readIORef (current v)
    unless (isBottom c) (act c)



-- | the function must satisfy the rule that if a >= b then f(a) >= f(b)

modifiedSuperSetOf :: (Fixable a, Fixable b) =>  Value b -> Value a -> (a -> b) -> IO ()
modifiedSuperSetOf (IV rv) (ConstValue cv) r = propegateValue (r cv) rv
modifiedSuperSetOf (IV rv) v2 r = addAction v2 (\x -> propegateValue (r x) rv)
modifiedSuperSetOf ConstValue {} _ _ =  fail "Fixer: You cannot modify a constant value"

isSuperSetOf :: Fixable a => Value a -> Value a -> IO ()
(IV rv) `isSuperSetOf` (ConstValue v2) = propegateValue v2 rv
(IV rv) `isSuperSetOf` v2 = addAction v2 (\x -> propegateValue x rv)
ConstValue {} `isSuperSetOf` _ =   fail "Fixer: You cannot modify a constant value"

-- | the function must satisfy the rule that if a >= b then f(a) implies f(b)
conditionalRule :: Fixable a => (a -> Bool) -> Value a -> Rules -> IO ()
conditionalRule cond v act = addAction v (\x -> if cond x then act else return ())

dynamicRule  :: Fixable a =>  Value a -> (a -> Rules) -> IO ()
dynamicRule v dr = addAction v dr

propegateValue :: Fixable a => a -> RvValue a -> IO ()
propegateValue p v = do
    if isBottom p then return () else do
    (modifyIORef (todo $ fixer v) (Set.insert $ MkFixable v))
    modifyIORef (pending v) (lub p)



readValue :: Value a -> IO a
readValue (IV v) = readIORef (current v)
readValue (ConstValue v) = return v


findFixpoint :: Fixer -> IO ()
findFixpoint Fixer { vars = vars, todo = todo } = do
    to <- readIORef todo
    vars <- readIORef vars
    putStrLn $ "Fixer: " ++ show (length vars)
    let f [] n | n > 0 = do
            vs <- readIORef todo
            writeIORef todo Set.empty
            putStr "(" >> putStr (show n) >> putStr ")" >> hFlush stdout
            f (Set.toList vs) 0
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
    f (Set.toList to) (0::Int)





