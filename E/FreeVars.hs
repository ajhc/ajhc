{-# OPTIONS -fglasgow-exts #-}
module E.FreeVars where

import FreeVars
import E.E
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Monoid
import GenUtil
import Data.Graph as G

-------------------------
-- finding free variables
-------------------------


getTyp tvr = tvrType tvr
getLitTyp (LitInt _ t) = t
getLitTyp (LitCons _ _ t) = t

instance FreeVars E IS.IntSet where
    freeVars e = IS.fromAscList (fsts . IM.toAscList $ freeVs e)
instance FreeVars E (Set.Set Int) where
    freeVars e = Set.fromAscList (fsts . IM.toAscList $ freeVs e)
instance FreeVars E [Int] where
    freeVars e =  IM.keys $ freeVs e
instance FreeVars E (IM.IntMap TVr) where
    freeVars = freeVs
instance FreeVars E (Set.Set TVr) where
    freeVars x = Set.fromAscList $ IM.elems (freeVs x)
instance FreeVars E [TVr] where
    freeVars x = IM.elems $ freeVars x
instance FreeVars (Alt E) (IM.IntMap TVr) where
    freeVars as@(Alt l e) = IM.unions $ freeVars (getLitTyp l):(freeVars e IM.\\ IM.fromList [ (tvrNum t,t) | t <- litBinds l]):( map (freeVars . getTyp) $ litBinds l)
instance FreeVars E t => FreeVars TVr t where
    freeVars tvr = freeVars (getTyp tvr :: E)
instance FreeVars (Alt E) (Set.Set Int) where
    freeVars as@(Alt l e) = Set.unions $ freeVars (getLitTyp l):(freeVars e Set.\\ Set.fromList [ tvrNum t | t <- litBinds l]):( map (freeVars . getTyp) $ litBinds l)
instance (FreeVars E x) => FreeVars (Lit TVr E) x where
    freeVars l =  mconcat $ freeVars (getLitTyp l :: E ):(map (freeVars . (getTyp :: TVr -> E) ) $ litBinds l)
instance FreeVars E (Map.Map Id (Maybe E)) where
    freeVars e = Map.fromAscList [ (v,Nothing) |  v <- IM.keys (freeVars e :: IM.IntMap TVr )]



freeVs ::  E -> IM.IntMap TVr
freeVs =   fv where
    (<>) = IM.union
    delete = IM.delete
    fv (EAp e1 e2) = fv e1 <> fv e2
    fv (EVar tvr@(TVr { tvrIdent =  ( i), tvrType =  t })) = IM.insert i tvr (fv t)
    fv (ELam (TVr { tvrIdent = i, tvrType = t}) e) =  (delete i $ fv e <> fv t)
    fv (EPi (TVr { tvrIdent =  i, tvrType = t}) e) =  (delete i $ fv e <> fv t)
    fv (ELetRec dl e) =  ((tl <> bl <> fv e) IM.\\ IM.fromList ll)  where
        (ll,tl,bl) = liftT3 (id,IM.unions,IM.unions) $ unzip3 $
            map (\(tvr@(TVr { tvrIdent = j, tvrType =  t}),y) -> ((j,tvr), fv t, fv y)) dl
    fv (EError _ e) = fv e
    fv (ELit l) = fvLit l
    fv (EPrim _ es e) = IM.unions $ fv e : map fv es
    fv (ECase e b as d) = IM.unions ( fv e:freeVars (getTyp  b):(IM.delete (tvrNum b) $ IM.unions (freeVars d:map freeVars as)  ):[])
    fv Unknown = IM.empty
    fv ESort {} = IM.empty
    fvLit (LitCons _ es e) = IM.unions $ fv e:map fv es
    fvLit l = freeVs (getLitTyp l)



-- | separate out recursive strongly connected components from a declaration list

decomposeDefns :: [(TVr, E)] -> [Either (TVr, E) [(TVr,E)]]
decomposeDefns bs = map f mp where
    mp = G.stronglyConnComp [ (v,i,freeVars t `mappend` freeVars e) | v@(TVr i t _ ,e) <- bs]
    f (AcyclicSCC v) = Left v
    f (CyclicSCC vs) = Right vs

-- | pull apart an ELet and separate out recursive strongly connected components from an ELet.
decomposeLet :: E ->  ([Either (TVr, E) [(TVr,E)]],E)
decomposeLet (ELetRec ds e) = (decomposeDefns ds,e)
decomposeLet e = ([],e)

