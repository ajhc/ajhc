module Info.Binary() where

import Binary
import Info.Info
import Atom
import PackedString
import qualified Data.Map as Map
import Info.Types
import Data.Dynamic
import GenUtil


data Binable = forall a . (Typeable a, Binary a) => Binable a

u :: (Typeable a, Binary a) => a
u = u

cb x = (packString (show (toDyn x)), Binable x)

binTable = Map.fromList [
    cb (u :: Arity),
    cb (u :: Properties),
    cb (u :: Atom)
    ]


putDyn :: BinHandle -> (PackedString,Dynamic,Binable) -> IO ()
putDyn h (ps,d,Binable (_::a)) = do
    put_ h ps
    put_ h (fromDyn d (error (show d)) :: a)

-- = case Map.lookup (packString (show d)) of
--    Just (Binable (x::a)) -> put_ h (case fromDynamic d of Just x -> x :: a)
--    Nothing -> return ()


getDyn h = do
    (ps::PackedString) <- get h
    b <- Map.lookup ps binTable
    case b of
        (Binable (_ :: a)) -> ((get h :: IO a) >>= return . toDyn)

instance Binary Info where
    put_ h (Info ds) = do
        let ds' = concatMap (\d -> do
                let ps = packString $ show d
                x <- Map.lookup ps binTable
                return (ps,d,x)
              )  ds
        put_ h (length ds')
        mapM_ (putDyn h) ds'
    get h = do
        (n::Int) <- get h
        xs <- replicateM n (getDyn h)
        return (Info xs)





