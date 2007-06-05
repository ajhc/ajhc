module Grin.HashConst where

import Control.Monad.State
import qualified Data.Map as Map

import Atom
import Grin.Grin
import Util.Graph

-- TODO tuples

data HcNode = HcNode {-# UNPACK #-} !Atom [Either Val Int]
    deriving(Show,Ord,Eq)

data HcHash = HcHash !Int (Map.Map HcNode Int)
    deriving(Show)

emptyHcHash = HcHash 1 Map.empty

{-# INLINE newConst #-}
{-# INLINE newConst' #-}
newConst :: MonadState HcHash m => Val -> m (Bool,Int)
newConst n = newConst' False n

newConst' :: MonadState HcHash m => Bool -> Val -> m (Bool,Int)
newConst' fuzzy n = f n where
    f (NodeC t vs) = do
        let g (Lit i ty)
                | fuzzy = return $ Left (Lit 0 ty)
                | otherwise = return $ Left (Lit i ty)
            g vp@(ValPrim _ _ ty)
                | fuzzy = return $ Left (Lit 0 ty)
                | otherwise = return $ Left vp
            g (Tag t)
                | fuzzy = return $ Left (Tag tagHole)
                | otherwise = return $ Left (Tag t)
            g x@(Var (V n) _) | n < 0  = return $ Left x
            g (Const n) = liftM (Right . snd) $ f n
            g n@NodeC {} = liftM (Right . snd) $ f n
            g e = error $ "HashConst.g: " ++ show e
        vs' <- mapM g vs
        let n = HcNode t vs'
        HcHash c h <- get
        case Map.lookup n h of
            Just n -> return (True,n)
            Nothing -> do
                let h' = Map.insert n c h
                put $ HcHash (c + 1) h'
                return (False,c)
    f _ = error "HashConst.newConst'"

toList :: HcHash -> [(HcNode,Int)]
toList (HcHash _ mp) = reverse ans where
    gr = newGraph (Map.toList mp) snd (gk . fst)
    gk (HcNode _ xs) = [ i | Right i <- xs]
    ans = topSort gr

