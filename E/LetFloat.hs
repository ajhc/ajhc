module E.LetFloat(
    atomizeApps,
    atomizeAp,
    floatOutward,
    programFloatInward,
    floatInward
  ) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Monoid
import Data.Typeable
import List  hiding(delete,insert)
import qualified Data.Map as Map

import DataConstructors
import Doc.PPrint
import E.E
import E.FreeVars
import E.Inline
import E.Program
import E.Subst
import E.Traverse
import E.TypeCheck
import E.Values
import GenUtil
import Info.Types
import Name.Id
import Name.Name
import Options
import Stats
import Support.CanType
import Support.FreeVars
import Util.SetLike
import Util.UniqueMonad()
import qualified Info.Info as Info
import qualified Util.Graph as G





atomizeApps ::
    Bool          -- ^ whether to atomize type arguments
    -> Program
    -> Program
atomizeApps atomizeTypes prog = ans where
    Identity ans = programMapBodies (return . atomizeAp mempty atomizeTypes (progDataTable prog)) prog

atomizeAp :: IdSet -> Bool -> DataTable -> E -> E
atomizeAp inscope atomizeTypes dataTable e = runReader (f e) inscope where
    f ELetRec { eDefs = [], eBody = e } = f e
    f ep@(ELam TVr { tvrIdent = i } _) = local (insert i) $ emapEG f return ep
    f el@ELetRec { eDefs = ds } = local (`mappend` fromList (map (tvrIdent . fst) ds)) $ emapEG f return el
    f ec@ECase {} = local (`mappend` fromList (map tvrIdent (caseBinds ec))) $ emapEG f return ec
    f (ELit lc@LitCons { litArgs = xs }) = mapM f xs >>= dl (\xs -> ELit lc { litArgs = xs })
    f ep@(EPi tvr@TVr {tvrIdent = i, tvrType = t} b) | i == 0 || i `notMember` freeIds b  = do
        t <- f t
        b <- f b
        dl (\ [t,b] -> EPi tvr { tvrIdent = 0, tvrType = t } b) [t,b]
    f ep@(EPi  TVr { tvrIdent = i } _) = local (insert i) $ emapEG f return ep
    f (EPrim n xs t) = mapM f xs >>= dl (\xs -> EPrim n xs t)
    f e = case fromAp e of
        (x,xs) -> do
            x <- emapEG f return x
            mapM f xs >>= dl (\xs -> foldl EAp x xs)
    dl build xs = do
        (fn,xs') <- h xs
        return $ fn (build xs')
    h :: [E] -> Reader IdSet (E -> E,[E])
    h (e:es) | isAtomic e = h es >>= \ (fn,es') -> return (fn,e:es')
    h (e:es) = do
        fvs <- ask
        let (var:_) = [ i | i <- newIds fvs]
            tvt = infertype dataTable e
            tv = tvr { tvrIdent = var, tvrType = tvt }
            fn = if getType tvt == eHash then eStrictLet tv e else eLetRec [(tv,e)]
        (fn',es') <- local (insert var) (h es)
        return (fn . fn',EVar tv:es')
    h [] = return (id,[])
    isAtomic :: E -> Bool
    isAtomic EVar {}  = True
--    isAtomic (EAp e v) | not atomizeTypes && isAtomic e && sortTypeLike v = True
    isAtomic e | not atomizeTypes && sortTypeLike e = True
    isAtomic e = isFullyConst e




fvBind (Left (_,fv)) = fv
fvBind (Right xs) = unions (snds xs)


canFloatPast t | sortKindLike . getType $ t = True
canFloatPast t | getType t == tWorldzh = True
canFloatPast t | getProperty prop_ONESHOT t = True
canFloatPast _ = False

{-# NOINLINE programFloatInward #-}
programFloatInward :: Program -> IO Program
programFloatInward prog = do
    let binds = G.scc $  G.newGraph [ (c ,freeVars c) | c <- progCombinators prog, combIdent c  `notElem` map combIdent epoints ] (combIdent . fst) (idSetToList . snd)
        epoints = [ c | c@Comb { combHead = x } <- progCombinators prog, (x `elem` progEntryPoints prog) || forceNoinline x || getProperty prop_INSTANCE x || getProperty prop_SPECIALIZATION x ]
        (oall,pints) = sepByDropPoint dpoints  (reverse binds)
        dpoints = map freeVars epoints
        nprog = progCombinators_s ([ combBody_u (\v -> fi c v y) c | (c,y) <- zip epoints pints] ++ [ combBody_u (\y -> floatInwardE y []) c | c <- dsBinds oall]) prog
        fi k = if getProperty prop_ONESHOT k then floatInwardE' else floatInwardE
    --mapM_ (putStrLn . pprint) (map fst $ dsBinds (concat pints))
    --Prelude.print (cupbinds binds)
    --Prelude.print dpoints
    --Prelude.putStrLn (pprint $ map fst (dsBinds binds))
    --Prelude.putStrLn (pprint $ (map fst $ dsBinds oall,map (\binds -> map fst $ dsBinds binds) pints))
    let mstats = mconcat [ Stats.singleton $ "FloatInward.{" ++ pprint n ++ "}" | n <- map combHead $ dsBinds (concat pints)]
        mstats' = mconcat [ Stats.singleton $ "FloatInward.all.{" ++ pprint n ++ "}" | n <- map combHead $ dsBinds oall]
        nstats = progStats prog `mappend` mstats `mappend` mstats'
    --nprog <- programMapBodies (return . floatInward) nprog
    return nprog { progStats = nstats }


--cupbinds bs = f bs where
--    f (Left ((t,_),fv):rs) = (tvrShowName t,fv):f rs
--    f (Right ds:rs) = f $ map Left ds ++ rs
--    f [] = []

floatInward ::
    E  -- ^ input term
    -> E  -- ^ output term
floatInward e = floatInwardE e [] where

floatInwardE :: E -> Binds -> E
floatInwardE e fvs = f e fvs where
    f ec@ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault =  d } xs = ans where
        ans = letRec p' $ caseUpdate ec { eCaseScrutinee = (f e pe), eCaseAlts = [ Alt l (f e pn) | Alt l e <- as | pn <- ps ], eCaseDefault = (fmap (flip f pd) d)}
        (p',_:pe:pd:ps) = sepByDropPoint (mconcat [freeVars l | Alt l _ <- as ]:freeVars e: tvrIdent b `delete` freeVars d :[freeVars a | a <- as ]) xs
    f ELetRec { eDefs = ds, eBody = e } xs = g (G.scc $  G.newGraph [ (bindComb d,freeVars $ bindComb d) | d <- ds ] (combIdent . fst) (idSetToList . snd) ) xs where
        g [] p' = f e p'
        g ((Left (comb@Comb { combHead = v, combBody = ev},fv)):xs) p = g xs (p0 ++ [Left (comb',freeVars comb')] ++ p') where
            comb' = combBody_s ev' comb
            ev' = if getProperty prop_ONESHOT v then floatInwardE' ev pv else f ev pv
            (p',[p0,pv,_]) = sepByDropPoint [(frest xs), freeVars comb, freeVars (tvrType v)] p
        g (Right bs:xs) p =  g xs (p0 ++ [Right [ let comb' = combBody_u (\ev -> f ev pv) comb in (comb',freeVars comb') | (comb,_) <- bs | pv <- ps ]] ++ p') where
            (p',_:p0:ps) = sepByDropPoint (freeVars (map (tvrType . combHead . fst) bs) :(frest xs):snds bs) p
        frest xs = mconcat (freeVars e:map fvBind xs)
    f e@ELam {} xs | all canFloatPast  ls = (foldr ELam (f b xs) ls) where
        (b,ls) = fromLam e
    f e@ELam {} xs = letRec unsafe_to_dup (foldr ELam (f b safe_to_dup) ls) where
        (unsafe_to_dup,safe_to_dup) = sepDupableBinds (freeVars ls) xs
        (b,ls) = fromLam e
    f e (Left (Comb { combHead = v', combBody = ev},_):xs)
        | (EVar v,as) <- fromAp e, v == v', not (tvrIdent v' `member` (freeVars as :: IdSet))  = f (runIdentity $ app (ev,as)) xs
    f e xs = letRec xs e
    letRec [] e = e
    letRec xs e = f (G.scc $ G.newGraph (concatMap G.fromScc xs) (combIdent . fst) (idSetToList . snd)) where
        f [] = e
        f (Left (te,_):rs) = eLetRec [combBind te] $ f rs
        f (Right ds:rs) = eLetRec (map (combBind . fst) ds) $ f rs

floatInwardE' e@ELam {} xs  = (foldr ELam (floatInwardE b xs) ls) where
    (b,ls) = fromLam e
floatInwardE' e xs = floatInwardE e xs

type FVarSet = IdSet
type Binds = [Either (Comb,FVarSet) [(Comb,FVarSet)]]

dsBinds bs = foldr ($) [] (map f bs) where
    f (Left (x,_)) = (x:)
    f (Right ds) = (map fst ds ++)

sepDupableBinds :: [Id] -> Binds -> (Binds,Binds)
sepDupableBinds fvs xs = partition ind xs where
    g = G.reachable (G.newGraph (concatMap G.fromScc xs) (combIdent . fst) (idSetToList . snd)) (fvs `mappend` unsafe_ones)
    uso = map (combIdent . fst) g
    unsafe_ones = concat [ map (combIdent . fst) vs | vs <- map G.fromScc xs,any (not . isCheap) (map (combBody . fst) vs)]
    ind x = any ( (`elem` uso) . combIdent . fst ) (G.fromScc x)


-- | seperate bindings based on whether they can be floated inward

sepByDropPoint ::
    [FVarSet]           -- ^ list of possible drop points
    -> Binds            -- ^ list of bindings and their free variables
    -> (Binds,[Binds])  -- ^ bindings seperated into those which must be dropped outside of all drop points, and those which can be floated inward into each branch
sepByDropPoint ds [] = ([], [ [] | _ <- ds ])
--sepByDropPoint ds fs' | sameShape1 xs ds && sum (length r:map length xs) <= length fs' = (r,xs) where
sepByDropPoint ds fs' = (r,xs) where
    (r,xs) = f fs'
    f [] = ([], [ [] | _ <- ds ])
    f (b:bs)
        | nu == 0 = f bs
        | nu == 1 =   case sepByDropPoint [ if v then d `mappend` fb' else d | (d,v) <- ds'  ] bs of
            (gb,ds'')  -> (gb, [ if v then b:d else d | d <- ds'' | (_,v) <- ds' ])
            -- (gb,ds'') | sameShape1 ds' ds'' -> (gb, [ if v then b:d else d | d <- ds'' | (_,v) <- ds' ])
        | otherwise = case sepByDropPoint [ d `mappend` fb' | d <- ds  ] bs of
            (gb,ds'')  -> (b:gb,ds'')
            --(gb,ds'') | sameShape1 ds'' ds -> (b:gb,ds'')
      where
        fb' = fvBind b
        ds' = [ (d,any  (`member` d) (fvDecls b)) | d <- ds ]
        nu = length (filter snd ds')
    fvDecls (Left (c,_)) = [combIdent c]
    fvDecls (Right ts) = [combIdent c | (c,_) <- ts ]


newtype Level = Level Int
    deriving(Eq,Ord,Enum,Show,Typeable)

newtype CLevel = CLevel Level
    deriving(Eq,Ord,Enum,Show,Typeable)

top_level = Level 0

--notFloatOut e = isAtomic e || whnfOrBot e
notFloatOut e = False

floatOutward :: Program -> IO Program
floatOutward prog = do
    -- set natural levels on all types
    let tl (t,e) imap = (tvrInfo_u (Info.insert top_level) t,g top_level e imap)
        g n e@ELam {} imap = foldr ELam (g n' b imap') ts' where
            (b,ts) = fromLam e
            n' = succ n
            ts' = map (tvrInfo_u (Info.insert n')) ts
            imap' = Map.fromList [ (tvrIdent t,n') | t <- ts] `Map.union` imap
        g n ec@ECase {} imap = runIdentity $ caseBodiesMapM (\e -> g' n e imap') ec { eCaseBind = m (eCaseBind ec), eCaseAlts = map ma (eCaseAlts ec) } where
            m t = tvrInfo_u (Info.insert n) t
            ma (Alt lc@LitCons { litName = n, litArgs = xs, litType = t }  b) = Alt lc { litArgs = map m xs } b
            ma a = a
            imap' = Map.fromList [ (tvrIdent t,n) | t <- caseBinds ec] `Map.union` imap
        g n ELetRec { eDefs = ds, eBody = e } imap = dds (map G.fromScc $ decomposeDs ds) [] e imap where
            dds (ts:rs) nrs e imap = dds rs (ts':nrs) e imap' where
                n' = maximum (Level 1:[ lup t | t <- fvs ])
                lup n = case Map.lookup n imap of
                    Just x -> x
                    Nothing -> error $ "LetFloat: could not find " ++ show tvr { tvrIdent = n }
                cl = CLevel n
                fvs = [ t | t <- freeVars (snds ts), t `notElem` (map (tvrIdent . fst) ts)]
                ts' = [(tvrInfo_u (Info.insert cl . Info.insert n') t,g n e imap') |  (t,e) <- ts]
                imap' = Map.fromList [ (tvrIdent t,n') | t <- fsts ts] `Map.union` imap
            dds [] nrs e imap = ELetRec (concat nrs) (g n e imap)
        g n e imap = runIdentity $ (emapE' (\e -> g' n e imap) e)
        g' n e imap = return $ g n e imap
    let imap = Map.fromList $ map (\x -> (x,top_level)) ([ tvrIdent t| (t,_) <-  programDs prog ] ++ idSetToList (progExternalNames prog `mappend` progSeasoning prog))
    prog <- flip programMapDs prog (\ (t,e) -> do
        e' <- letBindAll (progDataTable prog) (progModule prog) e
        return $ tl (t,e') imap)


    let dofloat ELetRec { eDefs = ds, eBody = e } = do
            e' <- dofloat e
            ds' <- mapM df ds
            return (ELetRec (concat ds') e')
        dofloat e@ELam {} = do
            let (b,ts) = fromLam e
                Just ln = Info.lookup (tvrInfo (head ts))
            (b',fs) <- censor (const []) $ listen (dofloat b)
            let (dh,de) = partition (\ (ll,bn) -> succ ll == ln) fs
            tell de
            return $ letRec (snds dh) (foldr ELam b' ts)
        dofloat e = emapE' dofloat e
        df (t,e) | Just (CLevel cl) <- lcl, cl /= nl = ans where
            ans = do
                e' <- dofloat e
                mtick $ "LetFloat.Full-Lazy.float.{" ++ maybeShowName t
                tell [(nl,(t,e'))]
                return []
            lcl = Info.lookup (tvrInfo t)
            Just nl = Info.lookup (tvrInfo t)
        df (t,e) = do
            e' <- dofloat e
            return [(t,e')]
--        dtl (t,ELetRec ds e) = do
--            (e',fs) <- runWriterT (dofloat e)
--            return $ (t,e'):snds fs
        dtl comb = do
            (e,fs) <- runWriterT (dofloat $ combBody comb)
            let (e',fs') = case e of
                    ELetRec { eDefs = ds, eBody = e } -> (e,ds++snds fs)
                    _ -> (e,snds fs)
                -- we imediatly float inward to clean up cruft and spurious outwards floatings
                (e'',fs'') = cDefs $ floatInward (ELetRec fs' e')
                cDefs (ELetRec ds e) = (e',ds ++ ds') where
                    (e',ds') = cDefs e
                cDefs e = (e,[])
            flip mapM_ (fsts $ fs'') $ \t -> do
                mtick $ "LetFloat.Full-Lazy.top_level.{" ++ maybeShowName t
            u <- newUniq
            let (fs''',sm') = unzip [ ((n,sm e),(t,EVar n)) | (t,e) <- fs'', let n = nn t ]
                sm = substLet sm'
                nn tvr = tvr { tvrIdent = toId $ lfName u (progModule prog) Val (tvrIdent tvr) }
            return $ combBody_s (sm e'') comb:map bindComb fs'''
    (cds,stats) <- runStatT (mapM dtl $ progCombinators prog)
    let nprog = progCombinators_s (concat cds) prog
    return nprog { progStats = progStats nprog `mappend` stats }


maybeShowName t = if '@' `elem` n then "(epheremal)" else n where
    n = tvrShowName t

lfName u modName ns x = case fromId x of
    Just y  -> toName ns (show modName, "fl@"++show y ++ "$" ++ show u)
    Nothing -> toName ns (show modName, "fl@"++show x ++ "$" ++ show u)


mapMSnd f xs = sequence [ (,) x `liftM` f y | (x,y) <- xs]


letBindAll ::
    DataTable  -- ^ the data table for expanding newtypes
    -> Module     -- ^ current module name
    -> E          -- ^ input term
    -> IO E
letBindAll  dataTable modName e = f e  where
    f :: E -> IO E
    f ELetRec { eDefs = ds, eBody = e } = do
        ds' <- mapMSnd f ds
        e' <- g e
        return $ ELetRec ds' e'
    f ec@ECase {} = do
        let mv = case eCaseScrutinee ec of
                EVar v -> subst (eCaseBind ec) (EVar v)
                _ -> id
        ec' <- caseBodiesMapM (fmap mv . g) ec
        scrut' <- g (eCaseScrutinee ec)
        return $ caseUpdate ec' { eCaseScrutinee = scrut' }
    f e@ELam {} = do
        let (b,ts) = fromLam e
        b' <- g b
        return (foldr ELam b' ts)
    f e = emapE' f e
    g e | notFloatOut e = return e
    g e | isUnboxed (getType e) = return e
    g e = do
        u <- newUniq
        let n = toName Val (show modName,"af@" ++ show u)
            tv = tvr { tvrIdent = toId n, tvrType = infertype dataTable e }
        e' <- f e
        return (ELetRec [(tv,e')] (EVar tv))



letRec [] e = e
letRec ds _ | flint && hasRepeatUnder fst ds = error "letRec: repeated variables!"
letRec ds e | flint && any (isUnboxed .tvrType . fst) ds = error "letRec: binding unboxed!"
letRec ds e = ELetRec ds e



