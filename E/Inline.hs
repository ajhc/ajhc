module E.Inline(
    app,
    programMapRecGroups,
    forceInline,
    programDecomposedDs,
    programMapProgGroups,
    programMapProgComponents,
    forceNoinline,
    baseInlinability
    ) where

import Control.Monad.Writer
import Data.Monoid

import Atom
import E.Annotate
import E.E
import E.Program
import E.Rules
import E.Subst
import E.Values
import GenUtil
import Info.Info(Info)
import Info.Types
import Name.Id
import Options
import Stats
import Support.FreeVars
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
    | otherwise  = member prop_INLINE props  || member prop_WRAPPER props || member prop_SUPERINLINE props
    where props = getProperties x

forceSuperInline :: HasProperties a => a -> Bool
forceSuperInline x
    | forceNoinline props = False
    | not (fopts FO.InlinePragmas) = False
    | otherwise = member prop_SUPERINLINE props
    where props = getProperties x

forceNoinline :: HasProperties a => a -> Bool
forceNoinline x
    | member prop_HASRULE props || member prop_NOINLINE props || member prop_PLACEHOLDER props = True
    | otherwise = False
    where props = getProperties x

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
    ds <- g [] imap $ programDecomposedDs prog
    return $ programSetDs ds prog





programDecomposedDs :: Program -> [Either (TVr, E) [(TVr,E)]]
programDecomposedDs prog = decomposeDs $ programDs prog

programComponents :: Program -> [[(TVr,E)]]
programComponents prog = components $ newGraph (programDs prog) (tvrIdent . fst) (toList . uncurry bindingFreeVars)

programSubProgram prog rec ds = programSetDs ds prog { progType = SubProgram rec, progEntryPoints = map fst ds }

programMapProgGroups :: Monad m =>
    IdMap (Maybe E)        -- ^ initial map to apply
    -> (Program -> m Program)
    -> Program
    -> m Program
programMapProgGroups imap f prog = do
    let g prog' rs imap (Left d:rds) = do
            [d'] <- annotateDs imap nann nann nann [d]
            nprog <- f (programSubProgram prog' False [d'])
            let nds = programDs nprog
            g (unames nds nprog) (nds:rs) (bm nds imap) rds
        g prog' rs imap (Right ds:rds) = do
            ds' <- annotateDs imap nann nann nann ds
            nprog <- f (programSubProgram prog' True ds')
            let imap' = bm nds imap
                smap = substMap'' $ fromList [ (tvrIdent x,Just $ EVar x) | (x,y) <- nds]
                nds = programDs nprog
                nds' = [ (x,smap y) | (x,y) <- nds]
            g (unames nds' nprog) (nds':rs) imap' rds
        g prog' rs _ [] = return $ (concat rs,prog')
        bm xs imap = fromList [ (tvrIdent t,Just $ EVar t) | (t,_) <- xs ] `union` imap
        nann _ = return
        prog' = prog { progStats = mempty }
        unames ds prog = prog { progExternalNames = progExternalNames prog `mappend` fromList [ tvrIdent t | (t,_) <- ds ] }
    (ds,prog'') <- g prog' [] imap $ programDecomposedDs prog
    return $ programSetDs ds prog { progStats = progStats prog `mappend` progStats prog'' }


programMapProgComponents :: Monad m =>
    (Program -> m Program)
    -> Program
    -> m Program
programMapProgComponents f prog = do
    let cs = programComponents prog
        prog' = prog { progStats = mempty, progType = MainComponent }
        g ds = f (programSetDs ds prog')
    ps <- mapM g (programComponents prog)
    return $ programSetDs (concatMap programDs ps) prog { progStats = progStats prog `mappend` (mconcat $ map progStats ps) }

