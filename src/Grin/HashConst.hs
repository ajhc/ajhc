module Grin.HashConst(newConst,HcHash(),HcNode(..),toList,emptyHcHash) where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Grin.Grin
import StringTable.Atom
import Util.Graph

-- TODO tuples

data HcNode = HcNode {-# UNPACK #-} !Atom [Either Val Int]
    deriving(Show,Ord,Eq)

data HcHash = HcHash !Int (Map.Map HcNode Int)
    deriving(Show)

emptyHcHash = HcHash 1 Map.empty

newConst :: MonadState HcHash m => Set.Set Atom -> Val -> m (Bool,Int)
newConst cpr n = f n where
    f (NodeC t vs) = do
        let g (Lit i ty)
                | otherwise = return $ Left (Lit i ty)
            g vp@(ValPrim _ _ ty)
                | otherwise = return $ Left vp
            g x@(Var (V n) _) | n < 0  = return $ Left x
            g n@(Const (NodeC _ [])) = return $ Left n
            g n@(NodeC _ []) = return $ Left n
            g n@(Const (NodeC a _)) | a `Set.member` cpr = return $ Left n
            g n@(NodeC a _) | a `Set.member` cpr  = return $ Left n
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
