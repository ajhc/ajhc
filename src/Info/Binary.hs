module Info.Binary(putInfo, Info.Binary.getInfo) where

import Data.Dynamic
import qualified Data.Map as Map

import C.FFI(FfiExport)
import Data.Binary
import E.CPR
import GenUtil
import Info.Info
import Info.Types
import Util.BitSet as BS
import qualified E.Demand

data Binable = forall a . (Typeable a, Binary a, Show a) => Binable a

u :: (Typeable a, Binary a) => a
u = u

newEntry x = Entry { entryThing = toDyn x, entryString = show x }

cb n x = (n, Binable x, typeOf x)

-- Note: the numbers here are part of the ABI of the serialized files.
-- If you change them then you must change the ABI version number in
-- Ho.Binary and invalidate all old files.
binTableValues =  [
    cb 2 (u :: E.CPR.Val),
    cb 3 (u :: FfiExport),
    cb 4 (u :: E.Demand.DemandSignature)
    ]

binTable :: Map.Map Word8 Binable
binTable = Map.fromList [ (n,x) | (n,x,_) <- binTableValues ]

revBinTable :: [(TypeRep,(Word8,Binable))]
revBinTable = [ (t,(n,x)) | (n,x,t) <- binTableValues ]

putDyn :: (Word8,Dynamic,Binable) -> Put
putDyn (ps,d,Binable (_::a)) = do
    put ps
    put (fromDyn d (error (show d)) :: a)

getDyn = do
    (ps::Word8) <- get
    case Map.lookup ps binTable of
        Just (Binable (_ :: a)) -> do
            x <- get :: Get a
            return $ newEntry x
        Nothing -> fail $ "getDyn: don't know how to read something of type: " ++ show ps

instance Binary Properties where
    put (Properties (EBS props)) = put (fromIntegral $ BS.toWord props :: Word32)
    get = (get :: Get Word32) >>= return . Properties . EBS . BS.fromWord . fromIntegral

instance Binary Info where
    put nfo = putInfo nfo
    get = Info.Binary.getInfo

putInfo :: Info.Info.Info -> Put
putInfo (Info p ds) = do
    let ds' = concatMap (\d -> do
            case Prelude.lookup (entryType d) revBinTable of
              Just (ps,x)  -> return (ps,entryThing d,x)
              Nothing -> fail "key not found"
          ) (Map.elems ds)
    put p
    putWord8 (fromIntegral $ length ds')
    mapM_ putDyn ds'

getInfo :: Get Info.Info.Info
getInfo = do
    p <- get
    n <- getWord8
    xs <- replicateM (fromIntegral n) getDyn
    return (Info p $ Map.fromList [ (entryType x,x) |x <- xs] )
