module E.Inline(
    basicDecompose,
    emapE,
    emapE',
    emapEG,
    app,
    emapE_,
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
import qualified Data.Graph as G
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

basicDecompose ::
    Maybe [Int]  -- ^ Just a set of values not to prune or nothing to not prune at all.
    -> Rules     -- ^ Rules for pruning
    -> E             -- ^ body for pruning info
    -> [(TVr,E)]     -- ^ incoming bindings
    -> [Either (TVr,E) [(TVr,E)]]     -- ^ bindings pruned and ordered by inlinability value
basicDecompose prune rules body ds = ans where
    zs = [ ((t,e), tvrNum t, bindingFreeVars t e ) |  (t,e) <- ds ]
    cg zs =  newGraph zs (\ (_,x,_) -> x) ( \ (_,_,x) -> x)
    tg = cg zs
    scc' = scc tg
    scc'' = case prune of
        Nothing -> scc'
        Just s -> scc $ cg $ reachable tg (freeVars body ++ s )
    ans = mapScc f scc''
    f (v,_,_) = v
    mapScc f = map g where
        g (Left x) = Left (f x)
        g (Right xs) = Right (map f xs)



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

app' (ELit (LitCons n xs t)) (a:as)  = do
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


emapE_ :: Monad m => (E -> m a) -> E -> m ()
emapE_ f e = emapEG f' f' e >> return () where
    f' e = f e >> return e
emapE f = emapEG f f
emapE' f = emapEG f return

emapEG f g e = z e where
    z (EAp aa ab) = do aa <- f aa;ab <- f ab; return $ EAp aa ab
    z (ELam aa ab) = do aa <- mapmTvr g aa; ab <- f ab; return $ ELam aa ab
    z (EPi aa ab) = do aa <- mapmTvr f aa; ab <- f ab; return $ EPi aa ab
    z (EVar aa) = do aa <- mapmTvr f aa; return $ EVar aa
    z (Unknown) = do return $ Unknown
    z (ESort aa) = do return $ ESort aa
    z (ELit (LitCons n es t)) = do t' <- g t; es' <- mapM f es; return $ ELit (LitCons n es' t')
    z (ELit aa) = do aa <- fmapM g aa; return $ ELit aa
    z (ELetRec aa ab) = do aa <- mapM (\x -> do x <- (do (aa,ab) <- return x; aa <- mapmTvr g aa;ab <- f ab;return (aa,ab)); return x) aa;ab <- f ab; return $ ELetRec aa ab
    z ec@ECase {} = do
        e' <- f $ eCaseScrutinee ec
        b' <- fmapM g (eCaseBind ec)
        as' <- mapM mapmAlt (eCaseAlts ec)
        d' <- fmapM f (eCaseDefault ec)
        t' <- g (eCaseType ec)
        return ECase { eCaseScrutinee =e', eCaseBind = b', eCaseAlts = as', eCaseDefault = d', eCaseType = t'}
    --    aa ab) = do aa <- f aa;ab <- mapM (\(x,y) -> do x <- fmapM f x; y <- f y; return (x,y)) ab; return $ ECase aa ab
    z (EPrim aa ab ac) = do ab <- mapM f ab;ac <- f ac; return $ EPrim aa ab ac
    z (EError aa ab) = do ab <- f ab; return $ EError aa ab
    mapmTvr = fmapM
    mapmAlt (Alt (LitCons n xs t) e) = do
        e' <- f e
        xs' <- mapM (fmapM g) xs
        t' <- g t
        return $ Alt (LitCons n xs' t') e'
    mapmAlt (Alt l e) = do
        e' <- f e
        l' <- fmapM g l
        return (Alt l' e')


instance Monoid Int where
    mempty = 0
    mappend = (+)
    mconcat = sum

instance HasSize E where
    size = eSize

eSize :: E -> Int
eSize e = n where
    (_, n) = runWriter (f e)
    f e@ELit {} = tell 1 >> return e
    f e@EPrim {} = tell 1 >> return e
    f e@EError {} = tell 1 >> return e
    f e = tell ( 1) >> emapE' f e

