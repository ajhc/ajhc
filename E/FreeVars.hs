{-# OPTIONS -fglasgow-exts #-}
module E.FreeVars(decomposeLet, decomposeDefns, freeIds) where

import Data.Graph as G
import Data.Monoid
import E.E
import GenUtil
import Name.Id
import Support.FreeVars
import Util.SetLike as S

-------------------------
-- finding free variables
-------------------------

getLitTyp (LitInt _ t) = t
getLitTyp LitCons { litType = t } = t

instance FreeVars E [TVr] where
    freeVars x = melems $ (freeVars x :: IdMap TVr)
instance FreeVars E [Int] where
    freeVars e =  idSetToList (freeVars e)

instance FreeVars E t => FreeVars TVr t where
    freeVars tvr = freeVars (tvrType tvr :: E)

instance (FreeVars E x) => FreeVars (Lit TVr E) x where
    freeVars l =  mconcat $ freeVars (getLitTyp l :: E ):(map (freeVars . (tvrType :: TVr -> E) ) $ litBinds l)


instance FreeVars (Alt E) IdSet where
    freeVars as@(Alt l e) = mconcat $ freeVars (getLitTyp l):(freeVars e S.\\ fromList [ tvrIdent t | t <- litBinds l]):(map (freeVars . tvrType) $ litBinds l)
instance FreeVars E IdSet where
    freeVars e = freeIds e

instance FreeVars (Alt E) (IdMap TVr) where
    freeVars as@(Alt l e) = mconcat $ freeVars (getLitTyp l):(freeVars e S.\\ fromList [ (tvrIdent t,t) | t <- litBinds l]):(map (freeVars . tvrType) $ litBinds l)
instance FreeVars E (IdMap TVr) where
    freeVars e = freeIdMap e
instance FreeVars E (IdMap (Maybe E)) where
    freeVars e = fmap (const Nothing) $ freeIdMap e

-- | separate out recursive strongly connected components from a declaration list

decomposeDefns :: [(TVr, E)] -> [Either (TVr, E) [(TVr,E)]]
decomposeDefns bs = map f mp where
    mp = G.stronglyConnComp [ (v,i,freeVars t `mappend` freeVars e) | v@(TVr i t _ ,e) <- bs]
    f (AcyclicSCC v) = Left v
    f (CyclicSCC vs) = Right vs

-- | pull apart an ELet and separate out recursive strongly connected components from an ELet.
decomposeLet :: E ->  ([Either (TVr, E) [(TVr,E)]],E)
decomposeLet ELetRec { eDefs = ds, eBody = e } = (decomposeDefns ds,e)
decomposeLet e = ([],e)

-- we export this to get a concrete type for free id sets.
freeIds ::  E -> IdSet
freeIds =   fv where
    (<>) = mappend
    fv (EAp e1 e2) = fv e1 <> fv e2
    --fv (EVar tvr@TVr { tvrIdent = i, tvrType = t }) = insert i (fv t)
    fv (EVar tvr@TVr { tvrIdent = i, tvrType = t }) = singleton i
    fv (ELam TVr { tvrIdent = i, tvrType = t} e) = delete i $ fv e <> fv t
    fv (EPi  TVr { tvrIdent = i, tvrType = t} e) = delete i $ fv e <> fv t
    fv ELetRec { eDefs = dl, eBody = e } =  ((tl <> bl <> fv e) S.\\ fromList ll)  where
        (ll,tl,bl) = liftT3 (id,mconcat,mconcat) $ unzip3 $
            map (\(tvr@(TVr { tvrIdent = j, tvrType =  t}),y) -> (j, fv t, fv y)) dl
    fv (EError _ e) = fv e
    fv (ELit l) = fvLit l
    fv (EPrim _ es e) = mconcat $ fv e : map fv es
    fv ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault = d, eCaseType = ty } = mconcat ( fv e:freeVars (tvrType  b):freeVars ty:(delete (tvrIdent b) $ mconcat (freeVars d:map freeVars as)  ):[])
    fv Unknown = mempty
    fv ESort {} = mempty
    fvLit LitCons { litArgs = es, litType = e } = mconcat $ fv e:map fv es
    fvLit l = fv (getLitTyp l)


-- we export this to get a concrete type for free id sets.
freeIdMap ::  E -> IdMap TVr
freeIdMap =   fv where
    (<>) = mappend
    fv (EAp e1 e2) = fv e1 <> fv e2
    --fv (EVar tvr@TVr { tvrIdent = i, tvrType = t }) = minsert i tvr (fv t)
    fv (EVar tvr@TVr { tvrIdent = i, tvrType = t }) = msingleton i tvr
    fv (ELam TVr { tvrIdent = i, tvrType = t} e) = mdelete i $ fv e <> fv t
    fv (EPi  TVr { tvrIdent = i, tvrType = t} e) = mdelete i $ fv e <> fv t
    fv ELetRec { eDefs = dl, eBody = e } =  ((tl <> bl <> fv e) S.\\ fromList ll)  where
        (ll,tl,bl) = liftT3 (id,mconcat,mconcat) $ unzip3 $
            map (\(tvr@(TVr { tvrIdent = j, tvrType =  t}),y) -> ((j,tvr), fv t, fv y)) dl
    fv (EError _ e) = fv e
    fv (ELit l) = fvLit l
    fv (EPrim _ es e) = mconcat $ fv e : map fv es
    fv ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault = d, eCaseType = ty } = mconcat ( fv e:freeVars (tvrType  b):freeVars ty:(mdelete (tvrIdent b) $ mconcat (freeVars d:map freeVars as)  ):[])
    fv Unknown = mempty
    fv ESort {} = mempty
    fvLit LitCons { litArgs = es, litType = e } = mconcat $ fv e:map fv es
    fvLit l = fv (getLitTyp l)



