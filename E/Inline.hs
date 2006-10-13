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
import Data.Monoid
import qualified Data.Graph as G

import Atom
import E.Annotate
import E.E
import E.Program
import E.Rules
import E.Subst
import E.Values
import GenUtil
import qualified Info.Info as Info
import Info.Info(Info)
import Info.Types
import Name.Id
import Options
import qualified FlagOpts as FO
import Stats
import Support.FreeVars
import Util.Graph
import Util.SetLike





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
    | Properties p <- Info.fetch (tvrInfo x) = member prop_INLINE p  || member prop_WRAPPER p || member prop_SUPERINLINE p

forceSuperInline x
    | forceNoinline x = False
    | not (fopts FO.InlinePragmas) = False
    | Properties p <- Info.fetch (tvrInfo x) =  member prop_SUPERINLINE p

forceNoinline x
    | Just (_x :: ARules) <- Info.lookup (tvrInfo x) = True
    | Properties p <- Info.fetch (tvrInfo x) = member prop_NOINLINE p || member prop_PLACEHOLDER p

app (e,[]) = return e
app (e,xs) = app' e xs

app' (ELit lc@LitCons { litName = n, litArgs = xs, litType = EPi ta tt }) (a:as)  = do
    mtick (toAtom $ "E.Simplify.typecon-reduce.{" ++ show n ++ "}" )
    app (ELit (lc { litArgs = xs ++ [a], litType = subst ta a tt }),as)
app' (ELit LitCons { litName = n, litArgs = es, litAliasFor = Just af }) bs@(_:_) = do
    mtick (toAtom $ "E.Simplify.newtype-reduce.{" ++ show n ++ "}" )
    app (foldl eAp af (es ++ bs),[])
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
    IdMap (Maybe E)        -- ^ initial map to apply
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
            let smap = substMap'' $ fromList [ (tvrIdent x,Just $ EVar x) | (x,y) <- nds]
                nds' = [ (x,smap y) | (x,y) <- nds]
            g (nds':rs) imap' rds
        g rs _ [] = return $ concat rs
        bm xs imap = fromList [ (tvrIdent t,Just $ EVar t) | (t,_) <- xs ] `union` imap
    ds <- g [] imap $ decomposeDs (programDs prog)
    return $ programSetDs ds prog

decomposeDs :: [(TVr, E)] -> [Either (TVr, E) [(TVr,E)]]
decomposeDs bs = map f mp where
    mp = G.stronglyConnComp [ (v,i, idSetToList $ bindingFreeVars t e) | v@(t@TVr { tvrIdent = i },e) <- bs]
    f (G.AcyclicSCC v) = Left v
    f (G.CyclicSCC vs) = Right vs





