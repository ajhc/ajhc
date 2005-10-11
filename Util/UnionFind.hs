module Util.UnionFind(
    Element,
    T,
    new,
    new_,
    updateW,
    getW,
    find,
    union,
    union_,
    fromElement
    ) where

import Control.Monad.Trans
import Data.IORef
import Data.Unique
import Monad(when,liftM)

data Element w a = Element a {-# UNPACK #-} !Int {-# UNPACK #-} !(IORef (Link w a))
data Link w a = Weight {-# UNPACK #-} !Int w | Next (Element w a)

type T = Element

new :: MonadIO m => w -> a -> m (Element w a)
new w x = liftIO $  do
    r <- newIORef (Weight 1 w)
    n <- liftM hashUnique newUnique
    return $ Element x n r

new_ :: MonadIO m =>  a -> m (Element () a)
new_ x = new () x


find :: MonadIO m => Element w a -> m (Element w a)
find x@(Element a _ r) = liftIO $  do
    e <- readIORef r
    case e of
        Weight _ _ -> return x
        Next next -> do
            last <- Util.UnionFind.find next
            when (next /= last) $ writeIORef r (Next last)
            return last

getW :: MonadIO m => Element w a -> m w
getW x = liftIO $ do
    Element _ _ r <- find x
    Weight _ w <- readIORef  r
    return w

updateW :: MonadIO m => (w -> w) -> Element w a -> m ()
updateW f x = liftIO $ do
    Element _ _ r <- find x
    Weight s w <- readIORef  r
    writeIORef r (Weight s (f w))



union :: MonadIO m => (w -> w -> w) -> Element w a -> Element w a -> m ()
union comb e1 e2 = liftIO $ do
    e1'@(Element _ _ r1) <- find e1
    e2'@(Element _ _ r2) <- find e2
    when (r1 /= r2) $ do
        Weight w1 x1 <- readIORef r1
        Weight w2 x2 <- readIORef r2
        if w1 <= w2 then do
            writeIORef r1 (Next e2')
            writeIORef r2 $! (Weight (w1 + w2) (comb x1 x2))
          else do
            writeIORef r1 $! (Weight (w1 + w2) (comb x1 x2))
            writeIORef r2 (Next e1')

union_ :: MonadIO m =>  Element () a -> Element () a -> m ()
union_ x y = union (\_ _ -> ()) x y



fromElement :: Element w a -> a
fromElement (Element a _ _) = a


instance Eq (Element w a) where
    Element _ x _ == Element _ y _ = x == y
    Element _ x _ /= Element _ y _ = x /= y

instance Ord (Element w a) where
    Element _ x _ `compare` Element _ y _ = x `compare` y
    Element _ x _ <= Element _ y _ = x <= y
    Element _ x _ >= Element _ y _ = x >= y

instance Show a => Show (Element w a) where
    showsPrec n (Element x _ _) = showsPrec n x


