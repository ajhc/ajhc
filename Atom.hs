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
import System.IO.Unsafe
import Data.IORef
import qualified Data.HashTable as HT
import qualified Data.IntMap as IM

import PackedString


instance Monoid Atom where
    mempty = toAtom nilPS
    mappend x y = toAtom $ appendPS (fromAtom x)  (fromAtom y)
    mconcat xs = toAtom $ concatPS (map fromAtom xs)

{-# NOINLINE table #-}
table :: HT.HashTable PackedString Atom
table = unsafePerformIO (HT.new (==) (fromIntegral . hashPS))

{-# NOINLINE reverseTable #-}
reverseTable :: IORef (IM.IntMap PackedString)
reverseTable = unsafePerformIO (newIORef IM.empty)

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

{-# NOINLINE fromPackedStringIO #-}
fromPackedStringIO :: PackedString -> IO Atom
fromPackedStringIO ps = HT.lookup table ps >>= \x -> case x of
    Just z -> return z
    Nothing -> do
        i <- peek intPtr
        poke intPtr (i + 2)
        let a = Atom i
        HT.insert table ps a
        modifyIORef reverseTable (IM.insert ((i - 1) `div` 2) ps)
        return a


-- The following are 'unwise' in that they may reveal internal structure that may differ between program runs

dumpAtomTable = do
    x <- HT.toList table
    mapM_ putStrLn [ show i ++ " " ++ show ps  | (ps,Atom i) <- sort x]


{-# NOINLINE intToAtom #-}
intToAtom :: Monad m => Int -> m Atom
intToAtom i | odd i && i > 0 = unsafePerformIO $ readIORef (i `seq` reverseTable) >>= \x -> case IM.member ((i-1) `div` 2) x of
    True -> return $ return $ Atom i
    False -> return $ fail $ "intToAtom: " ++ show i
intToAtom i = fail $ "intToAtom: " ++ show i

{-# NOINLINE toPackedString #-}
toPackedString :: Atom -> PackedString
toPackedString (Atom i) = unsafePerformIO $ readIORef (i `seq` reverseTable) >>= \x -> case IM.lookup ((i-1) `div` 2) x of
    Just ps -> return ps
    Nothing -> do
        x' <- readIORef reverseTable
        return $ error $ "toPackedString: " ++ show i ++ " " ++ (show (x,x'))

