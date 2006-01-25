{-# OPTIONS_GHC -fglasgow-exts #-}
-- find fixpoint of constraint problem

module Fixer.Fixer(
    Fixable(..),
    Value(),
    Rule(),
    Fixer(),
    addRule,
    ioToRule,
    conditionalRule,
    dynamicRule,
    findFixpoint,
    isSuperSetOf,
    modifiedSuperSetOf,
    newFixer,
    newValue,
    readValue,
    value
    ) where

import Data.IORef
import Data.Monoid
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

data Fixer  = Fixer {
    vars :: !(IORef [MkFixable]),
    todo :: !(IORef (Set.Set MkFixable))
    }


newFixer :: IO Fixer
newFixer = do
    v <- newIORef []
    t <- newIORef Set.empty
    return Fixer { vars = v, todo = t }

newtype Rule = Rule { unRule :: IO () }
    deriving(Typeable)

instance Monoid Rule where
    mempty = Rule (return ())
    mappend (Rule a) (Rule b) = Rule (a >> b)
    mconcat rs = Rule $ sequence_ $ map unRule rs

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
    propagateValue v rv
    return value


addAction :: Fixable a => Value a -> (a -> IO ())  -> IO ()
addAction (ConstValue n) act = act n
addAction (IV v) act = do
    modifyIORef (action v) (act:)
    c <- readIORef (current v)
    unless (isBottom c) (act c)

-- | add a rule to the current set
addRule :: Rule -> IO ()
addRule (Rule act) = act

-- | turn an IO action into a Rule
ioToRule :: IO () -> Rule
ioToRule act = Rule act

-- | the function must satisfy the rule that if a >= b then f(a) >= f(b)

modifiedSuperSetOf :: (Fixable a, Fixable b) =>  Value b -> Value a -> (a -> b) -> Rule
modifiedSuperSetOf (IV rv) (ConstValue cv) r = Rule $ propagateValue (r cv) rv
modifiedSuperSetOf (IV rv) v2 r = Rule $ addAction v2 (\x -> propagateValue (r x) rv)
modifiedSuperSetOf ConstValue {} _ _ =  Rule $ fail "Fixer: You cannot modify a constant value"

isSuperSetOf :: Fixable a => Value a -> Value a -> Rule
(IV rv) `isSuperSetOf` (ConstValue v2) = Rule $ propagateValue v2 rv
(IV rv) `isSuperSetOf` v2 = Rule $ addAction v2 (\x -> propagateValue x rv)
ConstValue {} `isSuperSetOf` _ = Rule $  fail "Fixer: You cannot modify a constant value"

-- | the function must satisfy the rule that if a >= b then f(a) implies f(b)
conditionalRule :: Fixable a => (a -> Bool) -> Value a -> Rule -> Rule
conditionalRule cond v (Rule act) = Rule $ addAction v (\x -> if cond x then act else return ())

dynamicRule  :: Fixable a =>  Value a -> (a -> Rule) -> Rule
dynamicRule v dr = Rule $ addAction v (unRule . dr)

propagateValue :: Fixable a => a -> RvValue a -> IO ()
propagateValue p v = do
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


-- some useful instances

instance Ord n => Fixable (Set.Set n)  where
    bottom = Set.empty
    isBottom = Set.null
    lub a b = Set.union a b
    minus a b = a Set.\\ b


instance Fixable Bool where
    bottom = False
    isBottom x = x == False
    lub a b = a || b
    minus True False = True
    minus False True = False
    minus True True = False
    minus False False = False

-- bottom is zero and the lub is the maximum of integer values, as in this is the lattice of maximum, not the additive one.
instance Fixable Int where
    bottom = 0
    isBottom = (0 ==)
    lub a b = max a b
    minus a b | a > b = a
    minus _ _ = 0

instance (Fixable a,Fixable b) => Fixable (a,b) where
    bottom = (bottom,bottom)
    isBottom (a,b) = isBottom a && isBottom b
    lub (x,y) (x',y') = (lub x x', lub y y')
    minus (x,y) (x',y') = (minus x x', minus y y')


-- the maybe instance creates a new bottom of nothing. note that (Just bottom) is a distinct point.
instance Fixable a => Fixable (Maybe a) where
    bottom = Nothing
    isBottom Nothing = True
    isBottom _ = False
    lub Nothing b = b
    lub a Nothing = a
    lub (Just a) (Just b) = Just (lub a b)
    minus (Just a) (Just b) = Just (minus a b)
    minus (Just a) Nothing = Just a
    minus Nothing _ = Nothing

