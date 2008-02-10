module Info.Binary(putInfo, Info.Binary.getInfo) where

import Data.Dynamic
import qualified Data.Map as Map
import Data.Word

import StringTable.Atom(HasHash(..))
import Data.Binary
import C.FFI(FfiExport)
import E.CPR
import GenUtil
import Info.Info
import Info.Types
import Util.BitSet as BS
import qualified E.Demand



data Binable = forall a . (Typeable a, Binary a, Show a) => Binable a

u :: (Typeable a, Binary a) => a
u = u

createTyp :: Typeable a => a -> Word32
createTyp x = hash32 $ (show (typeOf x))
newEntry x = Entry { entryThing = toDyn x, entryString = show x, entryType = typeOf x }

cb x = (createTyp x, Binable x)

binTable :: Map.Map Word32 Binable
binTable = Map.fromList [
    cb (u :: Properties),
    cb (u :: E.CPR.Val),
    cb (u :: FfiExport),
    cb (u :: E.Demand.DemandSignature)
    ]


putDyn :: (Word32,Dynamic,Binable) -> Put
putDyn (ps,d,Binable (_::a)) = do
    put ps
    put (fromDyn d (error (show d)) :: a)

-- = case Map.lookup (packString (show d)) of
--    Just (Binable (x::a)) -> put_ h (case fromDynamic d of Just x -> x :: a)
--    Nothing -> return ()


getDyn = do
    (ps::Word32) <- get
    case Map.lookup ps binTable of
        Just (Binable (_ :: a)) -> do
            x <- get :: Get a
            return $ newEntry x
        Nothing -> fail $ "getDyn: don't know how to read something of type: " ++ show ps

instance Binary Properties where
    put (Properties (EnumBitSet props)) = put (fromIntegral $ BS.toWord props :: Word32)
    get = (get :: Get Word32) >>= return . Properties . EnumBitSet . BS.fromWord . fromIntegral


instance Binary Info where
    put nfo = putInfo nfo
    get = Info.Binary.getInfo


putInfo :: Info.Info.Info -> Put
putInfo (Info ds) = do
    let ds' = concatMap (\d -> do
            let ps = hash32 $ (show $ entryType d)
            x <- Map.lookup ps binTable
            return (ps,entryThing d,x)
          ) ds
    putWord8 (fromIntegral $ length ds')
    mapM_ putDyn ds'

getInfo :: Get Info.Info.Info
getInfo = do
    n <- getWord8
    xs <- replicateM (fromIntegral n) getDyn
    return (Info  [ x | x <- xs])



