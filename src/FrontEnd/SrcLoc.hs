module FrontEnd.SrcLoc where

import Util.Std
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Binary

import PackedString

data SrcLoc = SrcLoc {
        srcLocFileName :: !PackedString,
        srcLocLine :: {-# UNPACK #-} !Int,
        srcLocColumn :: {-# UNPACK #-} !Int
        }
    deriving(Eq,Ord)
    {-! derive: update, Binary !-}

data SrcSpan = SrcSpan { srcSpanBegin :: !SrcLoc, srcSpanEnd :: !SrcLoc }
    deriving(Eq,Ord)
    {-! derive: update, Binary !-}

-- Useful bogus file names used to indicate where non file based errors are.
fileNameCommandLine = packString "(command line)"
fileNameUnknown = packString "(unknown)"
fileNameGenerated = packString "(generated)"

srcLocRelative
    :: SrcLoc    -- The forced location
    -> SrcLoc    -- saved natural location
    -> SrcLoc    -- current natural location
    -> SrcLoc    -- result
srcLocRelative floc nloc cloc = floc { srcLocLine = sll, srcLocColumn = slc } where
    sll = srcLocLine cloc - srcLocLine nloc + srcLocLine floc
    slc = srcLocColumn cloc - srcLocColumn nloc  + srcLocColumn floc

bogusASrcLoc = SrcLoc fileNameUnknown (-1) (-1)
eofSrcLoc    = SrcLoc fileNameUnknown maxBound (-1)
bogusSrcSpan = SrcSpan bogusASrcLoc bogusASrcLoc

instance Monoid SrcLoc where
    mempty = bogusASrcLoc
    mappend a b
        | a == bogusASrcLoc = b
        | otherwise = a

--------------------
-- haslocation class
--------------------

class HasLocation a where
    srcLoc :: a -> SrcLoc
    srcSpan :: a -> SrcSpan
    srcSpan x = bogusSrcSpan { srcSpanBegin = slx, srcSpanEnd = slx } where slx = srcLoc x
    srcLoc x = srcSpanBegin (srcSpan x)

instance HasLocation a => HasLocation [a] where
    srcLoc xs = mconcat (map srcLoc xs)

instance HasLocation SrcLoc where
    srcLoc x = x

instance HasLocation SrcSpan where
    srcSpan x = x

instance HasLocation (SrcLoc,SrcLoc) where
    srcSpan (x,y) = SrcSpan x y

instance HasLocation (Located a) where
    srcSpan (Located x _) = x

data Located x = Located SrcSpan x
    deriving(Ord,Eq,Functor,Foldable,Traversable)
    {-! derive: Binary !-}

instance Show a => Show (Located a) where
    showsPrec n (Located p x) =  showsPrec 10 x . showChar '@' . shows p

fromLocated :: Located x -> x
fromLocated (Located _ x) = x

located ss x = Located (srcSpan ss) x

-----------------------
-- srcloc monad classes
-----------------------

class (Applicative m,Monad m) => MonadSrcLoc m where
    getSrcLoc  :: m SrcLoc
    getSrcSpan :: m SrcSpan
    getSrcSpan = liftM srcSpan getSrcLoc
    getSrcLoc = liftM srcLoc getSrcSpan

class (MonadSrcLoc m,Applicative m) => MonadSetSrcLoc m where
    withSrcLoc' :: SrcLoc -> m a -> m a
    withSrcSpan' :: SrcSpan -> m a -> m a
    withSrcLoc' sl a = withSrcSpan (srcSpan sl) a
    withSrcSpan' ss a = withSrcLoc (srcLoc ss) a

instance MonadSetSrcLoc m => WithSrcLoc (m a) where
    withSrcLoc = withSrcLoc'
    withSrcSpan = withSrcSpan'

class WithSrcLoc a where
    withSrcLoc :: SrcLoc -> a -> a
    withSrcSpan :: SrcSpan -> a -> a
    withSrcLoc sl a | sl == bogusASrcLoc = a
    withSrcLoc sl a = withSrcSpan (srcSpan sl) a
    withSrcSpan ss a | ss == bogusSrcSpan = a
    withSrcSpan ss a = withSrcLoc (srcLoc ss) a

withLocation :: (HasLocation l,MonadSetSrcLoc m) => l -> m a -> m a
withLocation l = withSrcSpan (srcSpan l)

instance Monoid w => MonadSrcLoc (Writer w) where
    getSrcLoc = return mempty
instance Monoid w => MonadSetSrcLoc (Writer w) where
    withSrcLoc' _ a = a

instance MonadSrcLoc Identity where
    getSrcLoc = return mempty
instance MonadSetSrcLoc Identity where
    withSrcLoc' _ a = a

-----------------
-- show instances
-----------------

instance Show SrcLoc where
    show (SrcLoc fn l c) = unpackPS fn ++ f l ++ f c where
        f (-1) = ""
        f n | n == maxBound = "EOF"
        f n = ':':show n

instance Show SrcSpan where
    show SrcSpan { srcSpanBegin = sl1, srcSpanEnd = sl2 }
      | sl1 == sl2 = show sl1
      | slf sl1 == slf sl2 = slf sl1 ++ ":" ++ nums
      | otherwise =  show sl1 ++ "-" ++ show sl2 where
            nums | srcLocLine sl1 == srcLocLine sl2 = f sl1 ++ "-" ++ show (srcLocColumn sl2)
                 | otherwise = f sl1 ++ "-" ++ f sl2
            f s = show (srcLocLine s) ++ ":" ++ show (srcLocColumn s)
            slf = unpackPS . srcLocFileName

newtype SLM m a = SLM (ReaderT SrcSpan m a)
    deriving(Monad,MonadReader SrcSpan,Applicative,Functor)

runSLM :: SLM m a -> m a
runSLM (SLM t) = runReaderT t bogusSrcSpan

instance (Applicative m,Monad m) => MonadSetSrcLoc (SLM m) where
    withSrcSpan' ss a | ss == bogusSrcSpan = a
    withSrcSpan' ss (SLM a) = SLM $ local (const ss) a

instance (Applicative m,Monad m) => MonadSrcLoc (SLM m) where
    getSrcSpan = SLM ask
instance MonadSetSrcLoc IO where
    withSrcLoc' sl a = a
instance MonadSrcLoc IO where
    getSrcSpan = return bogusSrcSpan
