module Atom(
    Atom(),
    Atom.toString,
    FromAtom(..),
    ToAtom(..),
    atomIndex,
    dumpAtomTable,
    fromPackedStringIO,
    fromString,
    fromStringIO,
    intToAtom,
    toPackedString
    ) where

import Char
import Data.Generics
import Data.Monoid
import Foreign
import List(sort)
import qualified Data.HashTable as HT
import System.IO.Unsafe

import PackedString


instance Monoid Atom where
    mempty = toAtom nilPS
    mappend x y = toAtom $ appendPS (fromAtom x)  (fromAtom y)
    mconcat xs = toAtom $ concatPS (map fromAtom xs)

{-# NOINLINE table #-}
table :: HT.HashTable PackedString Atom
table = unsafePerformIO (HT.new (==) (fromIntegral . hashPS))

{-# NOINLINE reverseTable #-}
reverseTable :: HT.HashTable Int PackedString
reverseTable = unsafePerformIO (HT.new (==) (fromIntegral))

{-# NOINLINE intPtr #-}
intPtr :: Ptr Int
intPtr = unsafePerformIO (new 1)


newtype Atom = Atom Int
    deriving(Typeable, Data,Eq,Ord)

instance Show Atom where
    showsPrec _ atom = (toString atom ++)

instance Read Atom where
    readsPrec _ s = [ (fromString s,"") ]
    --readsPrec p s = [ (fromString x,y) |  (x,y) <- readsPrec p s]

toPackedString atom = atomToPS atom
toString atom = unpackPS $ toPackedString atom
atomIndex (Atom x) = x

{- these are separate in case operations are one-way -}
class ToAtom a where
    toAtom :: a -> Atom
class FromAtom a where
    fromAtom :: Atom -> a

instance ToAtom String where
    toAtom = fromString
instance FromAtom String where
    fromAtom = toString
instance FromAtom (String -> String) where
    fromAtom x = showsPS (fromAtom x)

instance ToAtom PackedString where
    toAtom x = unsafePerformIO $ fromPackedStringIO x
instance FromAtom PackedString where
    fromAtom = toPackedString

instance ToAtom Atom where
    toAtom x = x
instance FromAtom Atom where
    fromAtom x = x

instance ToAtom Char where
    toAtom x = toAtom [x]


fromString :: String -> Atom
fromString xs = unsafePerformIO $ fromStringIO xs

fromStringIO :: String -> IO Atom
fromStringIO cs = fromPackedStringIO (packString cs)

fromPackedStringIO :: PackedString -> IO Atom
fromPackedStringIO ps = HT.lookup table ps >>= \x -> case x of
    Just z -> return z
    Nothing -> do
        i <- peek intPtr
        poke intPtr (i + 2)
        let a = Atom i
        HT.insert table ps a
        HT.insert reverseTable i ps
        return a


-- The following are 'unwise' in that they may reveal internal structure that may differ between program runs

dumpAtomTable = do
    x <- HT.toList table
    mapM_ putStrLn [ show i ++ " " ++ show ps  | (ps,Atom i) <- sort x]


intToAtom :: Monad m => Int -> m Atom
intToAtom i = unsafePerformIO $  HT.lookup reverseTable i >>= \x -> case x of
    Just _ -> return (return $ Atom i)
    Nothing -> return $ fail $ "intToAtom: " ++ show i

atomToPS :: Atom -> PackedString
atomToPS (Atom i) = unsafePerformIO $  HT.lookup reverseTable i >>= \x -> case x of
    Just ps -> return ps
    Nothing -> return $ error $ "atomToPS: " ++ show i

