module E.Inline(
    app,
    programMapRecGroups,
    forceInline,
    programDecomposedDs,
    programMapProgGroups,
    forceNoinline,
    baseInlinability
    ) where

import Control.Monad.Writer
import Data.Monoid

import StringTable.Atom
import E.Annotate
import E.E
import E.Program
import E.Subst
import E.Values
import Support.FreeVars
import Info.Info(Info)
import Info.Types
import Name.Id
import Options
import Stats
import Util.Graph
import Util.SetLike
import qualified FlagOpts as FO
import qualified Info.Info as Info





-- | higher numbers mean we want to inline it more
baseInlinability t e
    | forceNoinline t = -15
    | forceSuperInline t = 10
    | forceInline t = 7
    | isAtomic e = 6
    | whnfOrBot e = 4
    | otherwise = 0

-- NOINLINE must take precidence because it is sometimes needed for correctness, while INLINE is surely an optimization.
forceInline :: HasProperties a => a -> Bool
forceInline x
    | forceNoinline props = False
    | not (fopts FO.InlinePragmas) = False
    | otherwise  = fromList [prop_INLINE,prop_WRAPPER,prop_SUPERINLINE] `intersects` props
    where props = getProperties x

forceSuperInline :: HasProperties a => a -> Bool
forceSuperInline x
    | forceNoinline props = False
    | not (fopts FO.InlinePragmas) = False
    | otherwise = member prop_SUPERINLINE props
    where props = getProperties x

forceNoinline :: HasProperties a => a -> Bool
forceNoinline x  = fromList [prop_HASRULE,prop_NOINLINE,prop_PLACEHOLDER] `intersects` getProperties x

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
    return $ caseUpdate ec' { eCaseType = t }
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
    -> ((Bool,[Comb]) -> m [Comb])  -- ^ bool is true if group is recursive.
    -> Program
    -> m Program
programMapRecGroups imap idann letann lamann f prog = do
    let g rs imap ((False,ds):rds) = do
            ds' <- annotateCombs imap idann letann lamann ds
            nds <- f (False,ds')
            g (nds:rs) (bm nds imap) rds
        g rs imap ((True,ds):rds) = do
            ds' <- annotateCombs imap idann letann lamann ds
            nds <- f (True,ds')
            let imap' = (bm nds imap)
            let smap = substMap' $ fromList [ (combIdent  x,EVar (combHead  x)) | x <- nds]
                nds' = [ combBody_u smap x | x <- nds]
            g (nds':rs) imap' rds
        g rs _ [] = return $ concat rs
        bm xs imap = fromList [ (combIdent c,Just $ EVar (combHead c)) | c <- xs ] `union` imap
    ds <- g [] imap $ programDecomposedCombs prog
    return $ programUpdate $ prog { progCombinators = ds }

programDecomposedCombs :: Program -> [(Bool,[Comb])]
programDecomposedCombs prog = map f $ scc g where
    g = newGraph (progCombinators prog) combIdent (toList . (freeVars :: Comb -> IdSet))
    f (Left c) = (False,[c])
    f (Right cs) = (True,cs)


programDecomposedDs :: Program -> [Either (TVr, E) [(TVr,E)]]
programDecomposedDs prog = decomposeDs $ programDs prog

programSubProgram prog rec ds = prog { progCombinators = ds, progType = SubProgram rec, progEntryPoints = map combHead ds }

programMapProgGroups :: Monad m =>
    IdMap (Maybe E)        -- ^ initial map to apply
    -> (Program -> m Program)
    -> Program
    -> m Program
programMapProgGroups imap f prog = do
    let g prog' rs imap ((False,ds):rds) = do
            ds' <- annotateCombs imap nann nann nann ds
            nprog <- f (programSubProgram prog' False ds')
            let nds = progCombinators nprog
            g (unames nds nprog) (nds:rs) (bm nds imap) rds
        g prog' rs imap ((True,ds):rds) = do
            ds' <- annotateCombs imap nann nann nann ds
            nprog <- f (programSubProgram prog' True ds')
            let imap' = bm nds imap
                smap = substMap' $ fromList [ (combIdent  x,EVar (combHead  x)) | x <- nds]
                nds = progCombinators nprog
                nds' = [ combBody_u smap x | x <- nds]
            g (unames nds' nprog) (nds':rs) imap' rds
        g prog' rs _ [] = return $ (concat rs,prog')
        bm xs imap = fromList [ (combIdent c,Just $ EVar (combHead c)) | c <- xs ] `union` imap
        nann _ = return
        unames ds prog = prog { progExternalNames = progExternalNames prog `mappend` fromList (map combIdent ds) }
    (ds,prog'') <- g prog { progStats = mempty } [] imap $ programDecomposedCombs prog
    return $ programUpdate $ prog { progCombinators = ds, progStats = progStats prog `mappend` progStats prog'' }


