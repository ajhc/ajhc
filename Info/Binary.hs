module Info.Binary(putInfo, Info.Binary.getInfo) where

import Data.Dynamic
import qualified Data.Map as Map

import Atom
import Binary
import C.FFI(FfiExport)
import E.CPR
import GenUtil
import Info.Info
import Info.Types
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


putDyn :: BinHandle -> (Atom,Dynamic,Binable) -> IO ()
putDyn h (ps,d,Binable (_::a)) = do
    put_ h ps
    put_ h (fromDyn d (error (show d)) :: a)

-- = case Map.lookup (packString (show d)) of
--    Just (Binable (x::a)) -> put_ h (case fromDynamic d of Just x -> x :: a)
--    Nothing -> return ()


getDyn h = do
    (ps::Atom) <- get h
    case Map.lookup ps binTable of
        Just (Binable (_ :: a)) -> do
            x <- get h :: IO a
            return $ newEntry x
        Nothing -> fail $ "getDyn: don't know how to read something of type: " ++ show ps

instance Binary Info where
    put_ h nfo = putInfo h nfo
    get h = Info.Binary.getInfo h


putInfo h (Info ds) = do
    let ds' = concatMap (\d -> do
            let ps = toAtom (show $ entryType d)
            x <- Map.lookup ps binTable
            return (ps,entryThing d,x)
          ) ds
    put_ h (length ds')
    mapM_ (putDyn h) ds'

getInfo h = do
    (n::Int) <- get h
    xs <- replicateM n (getDyn h)
    return (Info  [ x | x <- xs])



