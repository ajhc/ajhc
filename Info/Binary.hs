module Info.Binary(putInfo, Info.Binary.getInfo) where

import Data.Dynamic
import qualified Data.Map as Map

import Atom
import Data.Binary
import C.FFI(FfiExport)
import E.CPR
import Util.SetLike(toList,fromDistinctAscList)
import GenUtil
import Info.Info
import Info.Types
import Util.BitSet as BS
import qualified E.Demand



data Binable = forall a . (Typeable a, Binary a, Show a) => Binable a

u :: (Typeable a, Binary a) => a
u = u

createTyp :: Typeable a => a -> Atom
createTyp x = toAtom (show (typeOf x))
newEntry x = Entry { entryThing = toDyn x, entryString = show x, entryType = typeOf x }

cb x = (createTyp x, Binable x)

binTable = Map.fromList [
    cb (u :: Properties),
    cb (u :: E.CPR.Val),
    cb (u :: FfiExport),
    cb (u :: E.Demand.DemandSignature)
    ]


putDyn :: (Atom,Dynamic,Binable) -> Put
putDyn (ps,d,Binable (_::a)) = do
    put ps
    put (fromDyn d (error (show d)) :: a)

-- = case Map.lookup (packString (show d)) of
--    Just (Binable (x::a)) -> put_ h (case fromDynamic d of Just x -> x :: a)
--    Nothing -> return ()


getDyn = do
    (ps::Atom) <- get
    case Map.lookup ps binTable of
        Just (Binable (_ :: a)) -> do
            x <- get :: Get a
            return $ newEntry x
        Nothing -> fail $ "getDyn: don't know how to read something of type: " ++ show ps

instance Binary Properties where
    put (Properties (EnumBitSet props)) = put (BS.toWord props)
    get = get >>= return . Properties . EnumBitSet . BS.fromWord


instance Binary Info where
    put nfo = putInfo nfo
    get = Info.Binary.getInfo


putInfo :: Info.Info.Info -> Put
putInfo (Info ds) = do
    let ds' = concatMap (\d -> do
            let ps = toAtom (show $ entryType d)
            x <- Map.lookup ps binTable
            return (ps,entryThing d,x)
          ) ds
    put (length ds')
    mapM_ putDyn ds'

getInfo :: Get Info.Info.Info
getInfo = do
    (n::Int) <- get
    xs <- replicateM n getDyn
    return (Info  [ x | x <- xs])



