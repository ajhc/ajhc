module E.Inline(
    app,
    programMapRecGroups,
    forceInline,
    forceSuperInline,
    forceNoinline,
    baseInlinability,
    decomposeDs
    ) where

import Control.Monad.Writer
import Data.FunctorM
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Graph as G

import Atom
import E.E
import E.Program
import E.Rules
import E.Annotate
import E.Subst
import E.Values
import GenUtil
import Info.Info as Info
import Info.Types
import Options
import qualified FlagOpts as FO
import Stats
import Support.FreeVars
import Util.Graph
import Util.HasSize





-- | higher numbers mean we want to inline it more
baseInlinability t e
    | forceNoinline t = -15
    | forceSuperInline t = 10
    | forceInline t = 7
    | isAtomic e = 6
    | whnfOrBot e = 4
    | otherwise = 0




-- NOINLINE must take precidence because it is sometimes needed for correctness, while INLINE is surely an optimization.
forceInline x
    | forceNoinline x = False
    | not (fopts FO.InlinePragmas) = False
    | Properties p <- Info.fetch (tvrInfo x) = Set.member prop_INLINE p  || Set.member prop_WRAPPER p || Set.member prop_SUPERINLINE p

forceSuperInline x
    | forceNoinline x = False
    | not (fopts FO.InlinePragmas) = False
    | Properties p <- Info.fetch (tvrInfo x) =  Set.member prop_SUPERINLINE p

forceNoinline x
    | Just (_x :: ARules) <- Info.lookup (tvrInfo x) = True
    | Properties p <- Info.fetch (tvrInfo x) = Set.member prop_NOINLINE p || Set.member prop_PLACEHOLDER p

app (e,[]) = return e
app (e,xs) = app' e xs

app' (ELit (LitCons n xs t@EPi {})) (a:as)  = do
    mtick (toAtom $ "E.Simplify.typecon-reduce.{" ++ show n ++ "}" )
    app (ELit (LitCons n (xs ++ [a]) (eAp t a)),as)
app' (ELam tvr e) (a:as) = do
    mtick (toAtom "E.Simplify.beta-reduce")
    app (subst tvr a e,as)   -- TODO Fix quadradic substitution
    --app (eLet tvr a e,as)   -- TODO Fix quadradic substitution
app' (EPi tvr e) (a:as) = do
    mtick (toAtom "E.Simplify.pi-reduce")
    app (subst tvr a e,as)     -- Okay, types are small
app' ec@ECase {} xs = do
    mtick (toAtom "E.Simplify.case-application")
    let f e = app' e xs
    ec' <- caseBodiesMapM f ec
    let t = foldl eAp (eCaseType ec') xs
    return ec' { eCaseType = t }
app' (ELetRec ds e) xs = do
    mtick (toAtom "E.Simplify.let-application")
    e' <- app' e xs
    return $ eLetRec ds e'
app' (EError s t) xs = do
    mtick (toAtom "E.Simplify.error-application")
    return $ EError s (foldl eAp t xs)
app' e as = do
    return $ foldl EAp e as


-- | Map recursive groups, allowing an initial map to be passed in and it will
-- also propagate changes in the tvrInfo properly, and make sure nothing
-- shadows one of the global names.

programMapRecGroups :: Monad m =>
    Map.Map Id (Maybe E)        -- ^ initial map to apply
    -> (Id -> Info -> m Info)   -- ^ annotate based on Id map
    -> (E -> Info -> m Info)    -- ^ annotate letbound bindings
    -> (E -> Info -> m Info)    -- ^ annotate lambdabound bindings
    -> ((Bool,[(TVr,E)]) -> m [(TVr,E)])  -- ^ bool is true if group is recursive.
    -> Program
    -> m Program
programMapRecGroups imap idann letann lamann f prog = do
    let g rs imap (Left d:rds) = do
            [d'] <- annotateDs imap idann letann lamann [d]
            nds <- f (False,[d'])
            g (nds:rs) (bm nds imap) rds
        g rs imap (Right ds:rds) = do
            ds' <- annotateDs imap idann letann lamann ds
            nds <- f (True,ds')
            let imap' = (bm nds imap)
            let smap = substMap'' $ Map.fromList [ (tvrIdent x,EVar x) | (x,y) <- nds]
                nds' = [ (x,smap y) | (x,y) <- nds]
            g (nds':rs) imap' rds
        g rs _ [] = return $ concat rs
        bm xs imap = Map.fromList [ (tvrIdent t,Just $ EVar t) | (t,_) <- xs ] `Map.union` imap
    ds <- g [] imap $ decomposeDs (programDs prog)
    return $ programSetDs ds prog

decomposeDs :: [(TVr, E)] -> [Either (TVr, E) [(TVr,E)]]
decomposeDs bs = map f mp where
    mp = G.stronglyConnComp [ (v,i,bindingFreeVars t e) | v@(t@TVr { tvrIdent = i },e) <- bs]
    f (G.AcyclicSCC v) = Left v
    f (G.CyclicSCC vs) = Right vs





