module AtomName(testAtomName) where

import Control.Monad
import Data.List(sort,nub)
import Data.Monoid
import Name.Names
import PackedString
import StringTable.Atom
import System.IO.Unsafe
import Test.QuickCheck
import qualified Data.ByteString as BS

instance Arbitrary NameType where
    arbitrary = oneof $ map return [ minBound .. ]

deriving instance Bounded NameType

instance Arbitrary Module where
    arbitrary = g `fmap` arbitrary where
        g xs = f (filter (';' /=) xs)
        f "" = f "X"
        f s = toModule s

-- test things dealing with atoms, names, and efficient strings.
qc s p = quickCheck $ label s p

testAtomName = do
    testAtom
    testName
    testPackedString

testAtom = do
    let atomraw = unsafePerformIO $ addrToAtom_ "hello"# 5#
    print atomraw
    when (show atomraw /= "hello") $
        fail "atomRaw"
    when (show (mempty :: Atom) /= "") $
        fail "atomEmpty"
    when (mempty /= toAtom "") $
        fail "atomEmpty2"

    let prop_atomneq xs ys = (xs /= ys) == (a1 /= a2) where
            a1 = toAtom xs
            a2 = toAtom (ys :: String)
        prop_atomcompare xs ys = (xs `compare` ys) == (a1 `atomCompare` a2) where
            a1 = toAtom xs
            a2 = toAtom (ys :: String)
        -- prop_atomIndex (xs :: String) = intToAtom (fromAtom a) == Just a where
        --     a = toAtom xs
        prop_atomneq' xs ys = (xs `compare` ys) == (fromAtom a1 `compare` (fromAtom a2 :: BS.ByteString)) where
            a1 = toAtom xs
            a2 = toAtom (ys :: String)
        prop_atomint xs = null xs || an > 0  where
            an = fromAtom $ toAtom (xs :: String) :: Int
        -- prop_atomii xs = Just xs == fromAtom `fmap` (intToAtom an) where
        --     an = fromAtom $ toAtom (xs :: String) :: Int
        prop_aappend (xs,ys) = (toAtom xs `mappend` toAtom ys) == toAtom ((xs::String) ++ ys)
        prop_aappend' (xs,ys) = fromAtom (toAtom xs `mappend` toAtom ys) == ((xs::String) ++ ys)
    qc "atom.id" $ \xs -> fromAtom (toAtom xs) == (xs::String)
    qc "atom.eq" $ \xs -> (toAtom xs) == toAtom (xs::String)
--    qc "atom.index" prop_atomIndex
    qc "atom.neq" prop_atomneq
    qc "atom.compare" prop_atomcompare
    qc "atom.eq'" prop_atomneq'
    qc "atom.int" prop_atomint
 --   qc "atom.ii" prop_atomii
    qc "atom.aapend" prop_aappend

--strings = [ "foo", "foobar", "baz", "", "bob"]
strings =  ["h","n\206^um\198(","\186","yOw\246$\187x#",";\221x<n","\201\209\236\213J\244\233","\189eW\176v\175\209"]

appendPS :: PackedString -> PackedString -> PackedString
appendPS = mappend

testPackedString = do
    let prop_psid xs = unpackPS (packString xs) == (xs::String)
--        prop_pslen xs = lengthPS (packString xs) == length (xs::String)
        prop_psappend (xs,ys) = (packString xs `appendPS` packString ys) == packString ((xs::String) ++ ys)
        prop_psappend' (xs,ys) = unpackPS (packString xs `appendPS` packString ys) == ((xs::String) ++ ys)
        prop_sort xs = sort (map packString xs) == map packString (sort xs)
    qc "PackedString.psid" prop_psid
    qc "PackedString.sort"  prop_sort
    qc "PackedString.psappend"  prop_psappend
    qc "PackedString.psappend'"  prop_psappend'
--    quickCheck prop_pslen
--    print $ map packString strings
--    print $ sort $ map packString strings
--    print $ sort strings
 --   pshash "Hello"
--    pshash "Foo"
--    pshash "Bar"
--    pshash "Baz"
--    pshash ""
--    pshash "\2321\3221x.y\3421\2222"

--pshash xs = putStrLn $ xs ++ ": " ++ show (hashPS (packString xs ))

testName = do
    let nn x = (not (null x)) && ';' `notElem` x
        prop_acc t a b = nn a && nn b ==> let
            n = toName t (a::String,b::String)
            un = toUnqualified n
            in  nameType n == t && getModule n == Just (toModule a) && getModule un == Nothing && show un == b && show n == (a ++ "." ++ b)
    qc "name.tofrom" $ \t a b -> nn a && nn b ==> fromName (toName t (a::String,b::String)) == (t,(a,b))
    qc "name.nameparts" $ \t a b -> nn b ==> nameParts (toName t (a,b)) == (t,a,b)
    qc "name.nameparts2" $ \t a b ->  nn b ==> nameParts (toName t (a,b)) == (t,Just a,b)
    qc "name.toUnqualified" $ \t a b -> nn a && nn b ==> toUnqualified (toName t (a,b)) == toName t b
    qc "name.getModule" $ \ t a b -> nn a && nn b ==> getModule (toName t (a,b)) == Just (toModule a)
    qc "name.getModule2" $ \ t b -> nn b ==> getModule (toName t b) == Nothing
    qc "name.getIdent" $ \t a b ->  nn b ==> getIdent (toName t (a :: Maybe Module,b)) == b
    qc "name.setModule" $ \t a b c ->  nn b && nn c ==> setModule (toModule c) (toName t (a :: Maybe Module,b)) == toName t (c,b)
    qc "name.show" $ \ t a b -> nn a && nn b ==> show (toName t (a,b)) == a ++ "." ++ b
    qc "name.quote" $ \ t a b -> nn a && nn b ==> let n = toName t (a,b) in fromQuotedName (quoteName n) == Just n
    qc "name.noquote" $ \ t a b -> t /= QuotedName && nn b ==> collect t (fromQuotedName (toName t (a :: Maybe Module,b)) == Nothing)
    qc "name.acc" prop_acc
    qc "name.tup" $ \n -> n >= 0 ==> fromUnboxedNameTuple (unboxedNameTuple RawType n) == Just n
    qc "name.overlap" $ \t -> (isTypeNamespace t,isValNamespace t) /= (True,True)
