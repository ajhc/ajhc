{-# OPTIONS -fglasgow-exts #-}
module E.FreeVars(
    decomposeLet,
    decomposeDs,
    bindingFreeVars,
    caseUpdate,
    freeIds
    ) where

import Data.Monoid

import E.Type
import GenUtil
import Name.Id
import Support.FreeVars
import Util.SetLike as S
import Util.Graph
import qualified Info.Info as Info

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

decomposeDs :: [(TVr, E)] -> [Either (TVr, E) [(TVr,E)]]
decomposeDs bs = scc g where
    g = newGraph bs (tvrIdent . fst ) (toList . uncurry bindingFreeVars)

-- | pull apart an ELet and separate out recursive strongly connected components from an ELet.
decomposeLet :: E ->  ([Either (TVr, E) [(TVr,E)]],E)
decomposeLet ELetRec { eDefs = ds, eBody = e } = (decomposeDs ds,e)
decomposeLet e = ([],e)

caseUpdate :: E -> E
caseUpdate ec@ECase {} = ec { eCaseAllFV = fv ec } where
    fv ~(ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault = d, eCaseType = ty }) = mconcat (freeIds e:freeIds (tvrType  b):freeIds ty:(delete (tvrIdent b) $ mconcat (freeVars d:map freeVars as)  ):[])
caseUpdate e = e

-- we export this to get a concrete type for free id sets.
freeIds ::  E -> IdSet
freeIds =   fv where
    (<>) = mappend
    fv (EAp e1 e2) = fv e1 <> fv e2
    fv (EVar tvr@TVr { tvrIdent = i }) = singleton i <> freeVarsInfo (tvrInfo tvr)
    fv (ELam TVr { tvrIdent = i, tvrType = t} e) = delete i $ fv e <> fv t
    fv (EPi  TVr { tvrIdent = i, tvrType = t} e) = delete i $ fv e <> fv t
    fv ELetRec { eDefs = dl, eBody = e } =  ((tl <> bl <> fv e) S.\\ fromList ll)  where
        (ll,tl,bl) = liftT3 (id,mconcat,mconcat) $ unzip3 $
            map (\(tvr@(TVr { tvrIdent = j }),y) -> (j, freeVars tvr, fv y)) dl
    fv (EError _ e) = fv e
    fv (ELit l) = fvLit l
    fv (EPrim _ es e) = mconcat $ fv e : map fv es
    --fv ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault = d, eCaseType = ty } = mconcat ( fv e:freeVars (tvrType  b):freeVars ty:(delete (tvrIdent b) $ mconcat (freeVars d:map freeVars as)  ):[])
    fv ECase { eCaseAllFV = cfv } = cfv
    fv Unknown = mempty
    fv ESort {} = mempty
    fvLit LitCons { litArgs = es, litType = e } = mconcat $ fv e:map fv es
    fvLit l = fv (getLitTyp l)


-- we export this to get a concrete type for free id sets.
freeIdMap ::  E -> IdMap TVr
freeIdMap =   fv where
    (<>) = mappend
    fv (EAp e1 e2) = fv e1 <> fv e2
    fv (EVar tvr@TVr { tvrIdent = i }) = msingleton i tvr
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

-- | determine free variables of a binding site
instance FreeVars TVr IdSet where
    freeVars t = freeVars (tvrType t) `mappend` freeVarsInfo (tvrInfo t)

-- | this determines all free variables of a definition taking rules into account
bindingFreeVars :: TVr -> E -> IdSet
bindingFreeVars t e = freeVars t `mappend` freeVars e

freeVarsInfo nfo = maybe mempty freeVars (Info.lookup nfo :: Maybe ARules)
--instance FreeVars TVr (IdMap TVr) where
--    freeVars t = freeVars (tvrType t) `mappend` freeVars (Info.fetch (tvrInfo t) :: ARules)

instance FreeVars ARules IdSet where
    freeVars a = aruleFreeVars a

-- note, we include references to this combinator in its free variables.
instance FreeVars Comb IdSet where
    freeVars a = freeVars (combBody a) `union` (freeVars $ combRules a)

-- | we delete the free variables of the heads of a rule from the rule's free
-- variables. the reason for doing this is that the rule cannot fire if all its
-- heads are in scope, and if it were not done then many functions seem
-- recursive when they arn't actually.

instance FreeVars Rule IdSet where
    freeVars rule = freeVars (ruleBody rule) S.\\ (fromList (map tvrIdent $ ruleBinds rule) `mappend` ruleHeadFV rule)
--instance FreeVars Rule (IdMap TVr) where
--    freeVars rule = freeVars (ruleBody rule) S.\\ fromList [ (tvrIdent t,t) | t <- ruleBinds rule]


ruleHeadFV r = (S.insert (tvrIdent $ ruleHead r) $ freeVars (ruleArgs r)) S.\\ fromList (map tvrIdent $ ruleBinds r)

