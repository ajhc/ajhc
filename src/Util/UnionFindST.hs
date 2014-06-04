{-# LANGUAGE RankNTypes #-}
module Util.UnionFindST(
    Element,
    T,
    UF(),
    find,
    fromElement,
    getElements,
    getUnique,
    getW,
    liftST,
    new,
    new_,
    putW,
    runUF,
    union,
    union_,
    updateW
    ) where

import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef

newtype UF s a = UF (ReaderT (STRef s Int) (ST s) a)
    deriving(Monad,Functor)

runUF :: forall a . (forall s . UF s a)  -> a
runUF st = runST $ do
        ref <- newSTRef 0
        let rn = unUF st
            unUF (UF x) = x
        runReaderT rn ref

data Element s w a = Element a !Int {-# UNPACK #-} !(STRef s (Link s w a))
data Link s w a = Weight {-# UNPACK #-} !Int w [Element s w a] | Next (Element s w a)

type T = Element

newUnique :: UF s Int
newUnique = UF $ do
    ref <- ask
    u <- lift $ readSTRef ref
    let nu = u + 1
    lift $ writeSTRef ref nu
    return nu

new :: w -> a -> UF s (Element s w a)
new w x = do
    r <- liftST $ newSTRef (Weight 1 w [])
    n <- newUnique
    let ne = Element x n r
    liftST $ writeSTRef r (Weight 1 w [ne])
    return ne

new_ ::  a -> UF s (Element s () a)
new_ x = new () x

liftST = UF . lift

find ::  Element s w a -> UF s (Element s w a)
find x@(Element a _ r) = do
    e <- liftST $ readSTRef r
    case e of
        Weight {} -> return x
        Next next -> do
            last <- Util.UnionFindST.find next
            when (next /= last) $ liftST $ writeSTRef r (Next last)
            return last

getW ::  Element s w a -> UF s w
getW x = do
    Element _ _ r <- find x
    Weight _ w _ <- UF $ lift $ readSTRef  r
    return w

-- retrieve list of unified elements
getElements :: Element s w a -> UF s [Element s w a]
getElements x = do
    Element _ _ r <- find x
    Weight _ _ es <- liftST $ readSTRef  r
    return es

getUnique ::  Element s w a -> UF s Int
getUnique x = do
    Element _ u _ <- find x
    return u

-- update w returning the old value
updateW ::  (w -> w) -> Element s w a -> UF s w
updateW f x = do
    Element _ _ r <- find x
    Weight _ w _ <- liftST $ readSTRef  r
    liftST $ modifySTRef r (\ (Weight s w es) -> Weight s (f w) es)
    return w

-- puts a new w, returning old value
putW ::  Element s w a -> w -> UF s w
putW e w = updateW (const w) e

union ::  (w -> w -> w) -> Element s w a -> Element s w a -> UF s ()
union comb e1 e2 = do
    e1'@(Element _ _ r1) <- find e1
    e2'@(Element _ _ r2) <- find e2
    when (r1 /= r2) $ liftST $ do
        Weight w1 x1 es1 <- readSTRef r1
        Weight w2 x2 es2 <- readSTRef r2
        if w1 <= w2 then do
            writeSTRef r1 (Next e2')
            writeSTRef r2 $! (Weight (w1 + w2) (comb x1 x2) (es1 ++ es2))
          else do
            writeSTRef r1 $! (Weight (w1 + w2) (comb x1 x2) (es1 ++ es2))
            writeSTRef r2 (Next e1')

union_ ::   Element s () a -> Element s () a -> UF s ()
union_ x y = union (\_ _ -> ()) x y

fromElement :: Element s w a -> a
fromElement (Element a _ _) = a

instance Eq (Element s w a) where
    Element _ x _ == Element _ y _ = x == y
    Element _ x _ /= Element _ y _ = x /= y

instance Ord (Element s w a) where
    Element _ x _ `compare` Element _ y _ = x `compare` y
    Element _ x _ <= Element _ y _ = x <= y
    Element _ x _ >= Element _ y _ = x >= y

instance Show a => Show (Element s w a) where
    showsPrec n (Element x _ _) = showsPrec n x
