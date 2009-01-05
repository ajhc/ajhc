{-# OPTIONS_GHC -fglasgow-exts #-}
-- find fixpoint of constraint problem

{- 2009.01.05: Lemmih

This may be obvious to a lot of people but it certainly wasn't obvious to me.

The following module help you solve problems that involve iterating over
a piece of data until some steady-state (aka. a fixpoint) is found.

One example problem would be dead-code elimination. To remove all dead
functions and function arguments, we have to mark everything that
could possibly be alive (we necessarily have to be conservative).
This is done in two steps:
1) Walk through the code and make a note of all the dependencies
   (eg. function 'x' uses function 'y' and function 'z'). The dependencies
   are then handed over to the fixpoint solver.
2) The fixpoint solver iterate over all the data and use the dependencies
   to propagate the usage information. That is, if 'x' is used then 'y' and 'z'
   are as well. The next iteration will deal with the dependencies of 'y' and 'z'.

Once there's no more usage information to propagate, we know we've found our fixpoint.
There are several other problems that require fixpoint iteration. Perhaps the most
distinguished is the heap points-to analysis we use to eliminate eval/apply calls.

-}

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
    calcFixpoint,
    isSuperSetOf,
    modifiedSuperSetOf,
    newFixer,
    ioValue,
    newValue,
    readValue,
    readRawValue,
    value
    ) where

import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import Data.Typeable
import Data.Unique
import IO(hFlush, stdout, Handle, hPutStr)
import Monad
import qualified Data.Set as Set


-- | Fixable class, must satisfy the following rules
--
-- isBottom bottom == True
-- x `lub` x == x
-- x `lub` y == y `lub` x
-- x `lub` bottom == x
-- x `minus` bottom == x
-- bottom `minus` x == bottom
-- x `minus` y == z --> y `lub` z == x

class Fixable a where
    bottom :: a
    isBottom :: a -> Bool
    lub :: a -> a -> a
    minus :: a -> a -> a
    lte :: a -> a -> Bool
    lte x y = isBottom (x `minus` y)
    showFixable :: a -> String
    showFixable x | isBottom x = "."
                  | otherwise = "*"

data MkFixable = forall a . Fixable a => MkFixable (RvValue a)

data Fixer  = Fixer {
    vars :: !(IORef [MkFixable]),
    todo :: !(IORef (Set.Set MkFixable))
    }


newFixer :: MonadIO m => m Fixer
newFixer = liftIO $ do
    v <- newIORef []
    t <- newIORef Set.empty
    return Fixer { vars = v, todo = t }

newtype Rule = Rule { unRule :: IO () }
    deriving(Typeable)

instance Monoid Rule where
    mempty = Rule (return ())
    mappend (Rule a) (Rule b) = Rule (a >> b)
    mconcat rs = Rule $ sequence_ $ map unRule rs

instance Fixable a => Monoid (Value a) where
    mempty = value bottom
    mappend a b = UnionValue a b

data Value a = IOValue (IO (Value a)) | UnionValue (Value a) (Value a) | ConstValue a | IV (RvValue a)
    deriving(Typeable)

instance Fixable a => Show (Value a) where
    showsPrec _ (ConstValue a) = showString "<<" . showString (showFixable a) . showString ">>"
    showsPrec _ (UnionValue a b) = showString "<<" . shows a . shows b . showString ">>"
    showsPrec _ (IOValue _) = showString "<<IO>>"
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

-- | mainly for internal use
ioValue :: IO (Value a) -> Value a
ioValue iov = IOValue iov

newValue :: (MonadIO m,Fixable a) => Fixer -> a -> m (Value a)
newValue fixer@Fixer { vars = vars } v = liftIO $ do
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
addAction (UnionValue a b) act = addAction a act >> addAction b act
addAction (IOValue v) act = v >>= (`addAction` act)
addAction (IV v) act = do
    modifyIORef (action v) (act:)
    c <- readIORef (current v)
    unless (isBottom c) (act c)

-- | add a rule to the current set
addRule :: MonadIO m => Rule -> m ()
addRule (Rule act) = liftIO act

-- | turn an IO action into a Rule
ioToRule :: IO () -> Rule
ioToRule act = Rule act

-- | the function must satisfy the rule that if a >= b then f(a) >= f(b)

modifiedSuperSetOf :: (Fixable a, Fixable b) =>  Value b -> Value a -> (a -> b) -> Rule
modifiedSuperSetOf (IV rv) (ConstValue cv) r = Rule $ propagateValue (r cv) rv
modifiedSuperSetOf (IV rv) v2 r = Rule $ addAction v2 (\x -> propagateValue (r x) rv)
modifiedSuperSetOf (IOValue iov) v2 r = Rule $ iov >>= \v1 -> unRule $ modifiedSuperSetOf v1 v2 r
modifiedSuperSetOf (ConstValue vb) (ConstValue va)  f | f va `lte` vb =  Rule $ return ()
modifiedSuperSetOf ca@ConstValue {}  cb _ =  Rule $ fail ("Fixer.modifedSuperSetOf: You cannot modify a constant value:" ++ show(ca,cb))
modifiedSuperSetOf UnionValue {} _ _ =  Rule $ fail "Fixer: You cannot modify a union value"

isSuperSetOf :: Fixable a => Value a -> Value a -> Rule
(IV rv) `isSuperSetOf` (ConstValue v2) = Rule $ propagateValue v2 rv
(IV rv) `isSuperSetOf` v2 = Rule $ addAction v2 (\x -> propagateValue x rv)
(IOValue iov) `isSuperSetOf` v2 = Rule $ iov >>= unRule . (`isSuperSetOf` v2)
ConstValue v1 `isSuperSetOf` ConstValue v2 | v2 `lte` v1 =  Rule $ return ()
ConstValue {} `isSuperSetOf` _ = Rule $  fail "Fixer.isSuperSetOf: You cannot modify a constant value"
UnionValue {} `isSuperSetOf` _ = Rule $  fail "Fixer: You cannot modify a union value"

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


-- | read result, calculating fixpoint if needed
readValue :: (Fixable a,MonadIO m) => Value a -> m a
readValue (IV v) = liftIO $ do
    findFixpoint Nothing (fixer v)
    readIORef (current v)
readValue (IOValue iov) = liftIO iov >>= readValue
readValue (ConstValue v) = return v
readValue (UnionValue a b) = liftIO $ do
    a' <- readValue a
    b' <- readValue b
    return (lub a' b')

readRawValue :: (Fixable a,MonadIO m) => Value a -> m a
readRawValue (IV v) = liftIO $ do
    readIORef (current v)
readRawValue (IOValue iov) = liftIO iov >>= readRawValue
readRawValue (ConstValue v) = return v
readRawValue (UnionValue a b) = liftIO $ do
    a' <- readRawValue a
    b' <- readRawValue b
    return (lub a' b')

calcFixpoint :: MonadIO m => String -> Fixer -> m ()
calcFixpoint s fixer = findFixpoint (Just (s,stdout)) fixer

-- | find fixpoint, perhaps printing debugging information to specified handle. will not print anything if no calculation needed.
findFixpoint :: MonadIO m => Maybe (String,Handle) ->  Fixer -> m ()
findFixpoint msh@(~(Just (mstring,_))) Fixer { vars = vars, todo = todo } = liftIO $ do
    to <- readIORef todo
    if Set.null to then return () else do
    vars <- readIORef vars
    let f _ tl n | (tl::Int) `seq` n `seq` False = undefined
        f [] tl n | n > 0, tl /= 0 = do
            vs <- readIORef todo
            writeIORef todo Set.empty
            mputStr "(" >> mputStr (show n) >> mputStr ")" >> mFlush
            f (Set.toList vs) (tl - 1) 0
        f [] _ n | n > 0 = mputStr "[Aborting]\n" >> mFlush >> return ()
        f [] _ _ = mputStr "\n" >> mFlush >> return ()
        f (MkFixable v:vs) tl n = do
            p <- readIORef (pending v)
            c <- readIORef (current v)
            let diff = p `minus` c
            --if isBottom diff then f vs n else do
            if p `lte` c then f vs tl n else do
            as <- readIORef (action v)
            writeIORef (current v) (p `lub` c)
            writeIORef (pending v) bottom
            --putStr "["
            --putStr (showFixable diff)
            --putStr "]"
            mapM_ ($ diff) as
            f vs tl (n + 1)
        mputStr s = case msh of
            Nothing -> return ()
            Just (_,h) -> hPutStr h s
        mFlush = case msh of
            Nothing -> return ()
            Just (_,h) -> hFlush h
    mputStr $ "Finding fixpoint for " ++ mstring ++ ": " ++ "[" ++ show (Set.size to) ++ "]"
    mFlush
    f (Set.toList to) (-1) (0::Int)



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

