{-# LANGUAGE MagicHash #-}
import Data.Monoid
import Data.List(sort,nub)
import Control.Monad
import Test.QuickCheck
import qualified Data.Set as Set
import qualified List

import Text.Printf
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Info.Binary()
import Info.Types
import StringTable.Atom
import Support.MapBinaryInstance
import Util.HasSize
import Util.SetLike
import qualified Cmm.Op as Op
import qualified Info.Info as Info

import OptionsTest
import AtomName

{-# NOINLINE main #-}
main :: IO ()
main = do
    optionsTest
    testAtomName
    testProperties
    testHasSize
    testInfo
    testBinary
    testBinaryLEB

qc s p = quickCheck $ label s p

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

testBinaryLEB = do
    putStrLn "Testing Binary LEB"
    let num = 23948
    let bin = runPut $ putLEB128 num
    print bin
    let res = runGet getLEB128 bin
    printf "0x%x 0x%x\n" num res

    testleb 0x39
    testleb 0x73
    testleb 0x83

testleb num = do
    let bin = runPut $ putLEB128 num
    let res = runGet getLEB128 bin
    print bin
    printf "0x%x 0x%x\n" num res

deriving instance Enum Op.ValOp
--deriving instance Bounded Op.ValOp

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

--instance Arbitrary Char where
--    arbitrary     = Test.QuickCheck.choose ('\32', '\128')
--    coarbitrary c = variant (fromEnum c `rem` 4)
