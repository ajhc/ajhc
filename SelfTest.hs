module SelfTest(selfTest) where

import Data.Monoid
import List(sort)
import qualified List
import Monad
import qualified Data.Set as Set
import System.IO
import Test.QuickCheck

import Atom
import Binary
import Boolean.TestCases
import E.Arbitrary
import E.E
import GenUtil
import HsSyn
import Info.Binary()
import qualified Info.Info as Info
import Info.Types
import Name.Name
import Name.Names
import PackedString
import Util.ArbitraryInstances()
import Util.HasSize
import Util.SetLike
import qualified C.Generate



type Prop = Info.Types.Property

{-# NOINLINE selfTest #-}
selfTest :: [String] -> IO ()
selfTest _ = do
    putStrLn "Testing Boolean Library"
    testBoolean
    putStrLn "Testing Atom"
    quickCheck prop_atomid
    quickCheck prop_atomeq
    quickCheck prop_atomIndex
    quickCheck prop_atomneq
    quickCheck prop_atomneq'

    testProperties

    testPackedString
    testHasSize
    testName
    testInfo
    testBinary
    C.Generate.test
    -- testE

prop_atomid xs = fromAtom (toAtom xs) == (xs::String)
prop_atomeq xs = (toAtom xs) == toAtom (xs::String)
prop_atomneq xs ys = (xs /= ys) == (a1 /= a2) where
    a1 = toAtom xs
    a2 = toAtom (ys :: String)
prop_atomIndex (xs :: String) = intToAtom (atomIndex a) == Just a where
    a = toAtom xs
prop_atomneq' xs ys = (xs `compare` ys) == (toPackedString a1 `compare` toPackedString a2) where
    a1 = toAtom xs
    a2 = toAtom (ys :: String)


--strings = [ "foo", "foobar", "baz", "", "bob"]
strings =  ["h","n\206^um\198(","\186","yOw\246$\187x#",";\221x<n","\201\209\236\213J\244\233","\189eW\176v\175\209"]

testPackedString = do
    putStrLn "Testing PackedString"
    let prop_psid xs = unpackPS (packString xs) == (xs::String)
        prop_pslen xs = lengthPS (packString xs) == length (xs::String)
        prop_psappend (xs,ys) = (packString xs `appendPS` packString ys) == packString ((xs::String) ++ ys)
        prop_psappend' (xs,ys) = unpackPS (packString xs `appendPS` packString ys) == ((xs::String) ++ ys)
        prop_sort xs = sort (map packString xs) == map packString (sort xs)
    quickCheck prop_psid
    quickCheck prop_pslen
    doTime "prop_sort" $ quickCheck prop_sort
    quickCheck prop_psappend
    quickCheck prop_psappend'
    print $ map packString strings
    print $ sort $ map packString strings
    print $ sort strings
    pshash "Hello"
    pshash "Foo"
    pshash "Bar"
    pshash "Baz"
    pshash ""
    pshash "\2321\3221x.y\3421\2222"

pshash xs = putStrLn $ xs ++ ": " ++ show (hashPS (packString xs ))

testName = do
    putStrLn "Testing Name"
    let nn = not . null
    let prop_tofrom t a b = nn a && nn b ==> fromName (toName t (a::String,b::String)) == (t,(a,b))
        -- prop_pn t s = nn s ==> let (a,b) = fromName (parseName t s) in (a,b) == (t,s)
        prop_acc t a b = nn a && nn b ==> let
            n = toName t (a::String,b::String)
            un = toUnqualified n
            in  nameType n == t && getModule n == Just (Module a) && getModule un == Nothing && show un == b && show n == (a ++ "." ++ b)
        prop_tup n = n >= 0 ==> fromUnboxedNameTuple (unboxedNameTuple RawType n) == Just n
    quickCheck prop_tofrom
    -- quickCheck prop_pn
    quickCheck prop_acc
    quickCheck prop_tup

testProperties = do
    putStrLn "Testing Properties"
    let prop_list x xs = List.delete x xs == toList p where
            p = unsetProperty x ((fromList xs) :: Properties)
        prop_enum :: Prop -> Prop -> Bool
        prop_enum x y = (fromEnum x `compare` fromEnum y) == (x `compare` y)
    quickCheck prop_list
    quickCheck prop_enum

testBoolean = do
    quickCheck (\(x::Bool) -> prop_notnot x)
    quickCheck (\(x::Int) -> prop_notnot x)
    quickCheck (\(x::Int) -> prop_true x)
    quickCheck (\(x::Int) -> prop_false x)
    quickCheck (\(x::(Int,(Bool,Int))) -> prop_notnot x)
    quickCheck (\(x::(Int,(Bool,Int))) -> prop_true x)
    quickCheck (\(x::(Int,(Bool,Int))) -> prop_false x)
    quickCheck (\(x::(Int,(Bool,Int))) -> prop_false' x)
    quickCheck (\(x::[(Int,(Bool,Int))]) -> null x `trivial` prop_demorgan x)
    quickCheck (\(x::[(Int,(Bool,Int))]) -> null x `trivial` prop_demorgan' x)
    quickCheck $ prop_truefalse [3::Int] []
    quickCheck $ prop_truefalse (Just True) Nothing
    quickCheck $ prop_truefalse ((Right True),[3::Int]) (Left (), [])


testHasSize = do
    putStrLn "Testing HasSize"
    let prop_gt (xs,n) = sizeGT n (xs::[Int]) == (length xs > n)
        prop_gte (xs,n) = sizeGTE n (xs::[Int]) == (length xs >= n)
        prop_lte (xs,n) = sizeLTE n (xs::[Int]) == (length xs <= n)
    quickCheck prop_gt
    quickCheck prop_gte
    quickCheck prop_lte


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

putFile fn a = do
    h <- openBinaryFile fn WriteMode
    bh <- openBinIO h
    put_ bh a
    hClose h

getFile fn = do
    h <- openBinaryFile fn ReadMode
    bh <- openBinIO h
    b <- get bh
    hClose h
    return b


testBinary = do
    let test = ("hello",3::Int)
        fn = "/tmp/jhc.test.bin"
    putStrLn "Testing Binary"
    putFile fn test
    x <- getFile fn
    if (x /= test) then fail "Test Failed" else return ()
    let fn = "/tmp/jhc.info.bin"
        t = (singleton prop_INLINE) `mappend` fromList [prop_WORKER,prop_SPECIALIZATION]
        t :: Properties
        nfo = (Info.insert "food" $ Info.insert t mempty)
        nf = (nfo, "Hello, this is a test", Set.fromList ['a' .. 'f'])
    print nf
    putFile fn nf
    x@(nfo,_,_) <- getFile fn
    print $ x `asTypeOf` nf
    z <- Info.lookup nfo
    if (z /= t) then fail "Info Test Failed" else return ()








instance Arbitrary NameType where
    arbitrary = oneof $ map return [ TypeConstructor .. ]

instance Arbitrary Info.Types.Property where
    arbitrary = oneof $ map return [ minBound .. ]

instance Arbitrary Properties where
    arbitrary = fromList `fmap` arbitrary
