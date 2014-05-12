import Data.Monoid
import List(sort,nub)
import Monad
import System.IO
import Test.QuickCheck
import qualified Data.Set as Set
import qualified List

import Data.Binary
import E.Arbitrary
import E.E
import FrontEnd.HsSyn
import GenUtil
import Info.Binary()
import Info.Types
import Name.Name
import Name.Names
import PackedString
import StringTable.Atom
import Util.HasSize
import Util.SetLike
import qualified C.Generate
import qualified Cmm.Op as Op
import qualified Data.ByteString as BS
import qualified Info.Info as Info

{-# NOINLINE main #-}
main :: IO ()
main = do
    testAtom
    testProperties
    testPackedString
    testHasSize
    testName
    testInfo
    testBinary

qc s p = quickCheck $ label s p

testAtom = do
    let prop_atomneq xs ys = (xs /= ys) == (a1 /= a2) where
            a1 = toAtom xs
            a2 = toAtom (ys :: String)
        prop_atomIndex (xs :: String) = intToAtom (fromAtom a) == Just a where
            a = toAtom xs
        prop_atomneq' xs ys = (xs `compare` ys) == (fromAtom a1 `compare` (fromAtom a2 :: BS.ByteString)) where
            a1 = toAtom xs
            a2 = toAtom (ys :: String)
        prop_atomint xs = an > 0 && odd an where
            an = fromAtom $ toAtom (xs :: String) :: Int
        prop_atomii xs = Just xs == fromAtom `fmap` (intToAtom an) where
            an = fromAtom $ toAtom (xs :: String) :: Int
        prop_aappend (xs,ys) = (toAtom xs `mappend` toAtom ys) == toAtom ((xs::String) ++ ys)
        prop_aappend' (xs,ys) = fromAtom (toAtom xs `mappend` toAtom ys) == ((xs::String) ++ ys)
    qc "atom.id" $ \xs -> fromAtom (toAtom xs) == (xs::String)
    qc "atom.eq" $ \xs -> (toAtom xs) == toAtom (xs::String)
    qc "atom.index" prop_atomIndex
    qc "atom.neq" prop_atomneq
    qc "atom.eq'" prop_atomneq'
    qc "atom.int" prop_atomint
    qc "atom.ii" prop_atomii
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

testProperties = do
    let prop_list x xs = sort (List.delete x $ nub xs) == toList p where
            p = unsetProperty x ((fromList xs) :: Properties)
        prop_enum :: Info.Types.Property -> Info.Types.Property -> Bool
        prop_enum x y = (fromEnum x `compare` fromEnum y) == (x `compare` y)
    qc "prop.list" prop_list
    qc "prop.enum" prop_enum

testHasSize = do
    let prop_gt (xs,n) = sizeGT n (xs::[Int]) == (length xs > n)
        prop_gte (xs,n) = sizeGTE n (xs::[Int]) == (length xs >= n)
        prop_lte (xs,n) = sizeLTE n (xs::[Int]) == (length xs <= n)
    qc "hasSize.gt" prop_gt
    qc "hasSize.gte" prop_gte
    qc "hasSize.lte" prop_lte

testInfo = do
    putStrLn "Testing Info"
    i <- return mempty
    unless (Info.lookup i == (Nothing :: Maybe Int)) $ fail "test failed..."
    i <- return $ Info.insert (3 :: Int) i
    unless (Info.lookup i == (Just 3 :: Maybe Int)) $ fail "test failed..."
    unless (Info.fetch (Info.insert (5 :: Int) i) == ([] :: [Int])) $ fail "test failed..."

    let x = Properties mempty
        x' = setProperty prop_METHOD $ setProperty prop_INLINE x
    print (x',getProperty prop_METHOD x', getProperty prop_INSTANCE x')
    let x'' = setProperty prop_INSTANCE $ unsetProperty prop_METHOD x'
    print (x'',getProperty prop_METHOD x'', getProperty prop_INSTANCE x'')

    let x = Info.empty
        x' = setProperty prop_METHOD $ setProperty prop_INLINE x
    print (x',getProperty prop_METHOD x', getProperty prop_INSTANCE x')
    let x'' = setProperty prop_INSTANCE $ unsetProperty prop_METHOD x'
    print (x'',getProperty prop_METHOD x'', getProperty prop_INSTANCE x'')
    print (getProperties x')

testBinary = do
    let test = ("hello",3::Int,toAtom "Up and Atom!")
        fn = "/tmp/jhc.test.bin"
    putStrLn "Testing Binary"
    encodeFile fn test
    x <- decodeFile fn
    if (x /= test) then fail "Test Failed" else return ()
    print x
    let fn = "/tmp/jhc.info.bin"
        t = (singleton prop_INLINE) `mappend` fromList [prop_WORKER,prop_SPECIALIZATION]
        t :: Properties
        nfo = (Info.insert "food" $ Info.insert t mempty)
        nf = (nfo, "Hello, this is a test", Set.fromList ['a' .. 'f'])
    print nf
    encodeFile fn nf
    x@(nfo,_,_) <- decodeFile fn
    print $ x `asTypeOf` nf
    z <- Info.lookup nfo
    if (z /= t) then fail "Info Test Failed" else return ()

deriving instance Bounded NameType
deriving instance Enum Op.ValOp
--deriving instance Bounded Op.ValOp

instance Arbitrary NameType where
    arbitrary = oneof $ map return [ minBound .. ]
instance Arbitrary Op.BinOp where
    arbitrary = oneof $ map return [ minBound .. ]
instance Arbitrary Op.UnOp where
    arbitrary = oneof $ map return [ minBound .. ]
instance Arbitrary Op.ConvOp where
    arbitrary = oneof $ map return [ minBound .. ]
instance Arbitrary Op.ValOp where
    arbitrary = oneof $ map return [ minBound .. ]

instance Arbitrary Info.Types.Property where
    arbitrary = oneof $ map return [ minBound .. ]

instance Arbitrary Properties where
    arbitrary = fromList `fmap` arbitrary

instance Arbitrary Module where
    arbitrary = g `fmap` arbitrary where
        g xs = f (filter (';' /=) xs)
        f "" = f "X"
        f s = toModule s

--instance Arbitrary Char where
--    arbitrary     = Test.QuickCheck.choose ('\32', '\128')
--    coarbitrary c = variant (fromEnum c `rem` 4)
