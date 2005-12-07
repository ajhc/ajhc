module FrontEnd.SrcLoc where

import Data.Monoid
import Data.Generics
import Binary


data SrcLoc = SrcLoc { srcLocFileName :: String, srcLocLine :: !Int, srcLocColumn :: !Int}
    deriving(Data,Typeable,Eq,Ord)
    {-! derive: update, GhcBinary !-}

data SrcSpan = SrcSpan { srcSpanBegin :: !SrcLoc, srcSpanEnd :: !SrcLoc }
    deriving(Data,Typeable,Eq,Ord)
    {-! derive: update !-}

bogusASrcLoc = SrcLoc "bogus#" (-1) (-1)
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

data Located x = Located SrcSpan x
    deriving(Ord,Show,Data,Typeable,Eq)

instance HasLocation (Located a) where
    srcSpan (Located x _) = x

-----------------
-- show instances
-----------------

instance Show SrcLoc where
    show (SrcLoc fn l c) = fn ++ f l ++ f c where
        f (-1) = ""
        f n = ':':show n

instance Show SrcSpan where
    show SrcSpan { srcSpanBegin =  sl1, srcSpanEnd = sl2 }
      | sl1 == sl2 = show sl1
      | otherwise = show sl1 ++ "-" ++ show sl2

