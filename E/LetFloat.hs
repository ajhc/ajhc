module E.LetFloat(
    atomizeAp,
    coalesceLets,
    floatOutward,
    floatInward
  ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Monoid
import Data.Typeable
import List  hiding(delete,insert)
import qualified Data.Map as Map

import Atom
import DataConstructors
import Doc.PPrint
import E.E
import E.Subst
import E.Program
import E.Inline
import Info.Info as Info hiding(member,delete)
import E.Rules
import E.Traverse
import E.TypeCheck
import E.Values
import GenUtil
import Name.Name
import Options
import qualified CharIO as C
import qualified Util.Graph as G
import Stats
import Support.CanType
import Support.FreeVars
import Util.UniqueMonad
import Util.SetLike
import Name.Id



doLetRec stats [] e = return e
doLetRec stats ds _ | flint && hasRepeatUnder fst ds = error "doLetRec: repeated variables!"
doLetRec stats ds e = return $ ELetRec ds e

varElim :: Stats -> Int -> IO ()
varElim stats n = do
    ticks stats n (toAtom "E.Simplify.var-elimination")


atomizeAp ::
    Bool          -- ^ whether to atomize type arguments
    -> DataTable  -- ^ the data table for expanding newtypes
    -> Stats      -- ^ statistics
    -> Module     -- ^ current module name
    -> E          -- ^ input term
    -> IO E
atomizeAp atomizeTypes dataTable stats modName e = f e  where
    f :: E -> IO E
    f e = do
        (x,ds) <- g e
        ds' <- sequence [  f y >>= return . (,) x | (x,y) <- ds ]
        doLetRec stats ds' x
    g,h :: E -> IO (E,[(TVr,E)])
    g (ELetRec ds e) = do
        e' <- f e
        return (e',ds)
    g (ELam tvr e) = do
        e' <- f e
        return (ELam tvr e',[])
    g (ELit (LitCons n xs t)) = do
        (xs',dss) <- fmap unzip (mapM h xs)
        return (ELit (LitCons n xs' t), concat dss)
    g e@ELit {} = return (e,[])
    g e@EError {} = return (e,[])
    g ep@(EPi tvr@TVr {tvrIdent = i, tvrType = t} b) | i == 0 || i `notElem` freeVars b  = do
        ([t',b'],dss) <- fmap unzip (mapM h [t,b])
        return (EPi tvr { tvrIdent = 0, tvrType = t' } b', concat dss)
    g (EPrim n xs t) = do
        (xs',dss) <- fmap unzip (mapM h xs)
        return (EPrim n xs' t, concat dss)
    g ec@ECase { eCaseScrutinee = e } = do
        ec' <- caseBodiesMapM f ec
        e' <- f e
        return (ec' { eCaseScrutinee = e' },[])
    g e = case fromAp e of
        (EVar x,xs) -> do
            (xs',dss) <- fmap unzip (mapM h xs)
            return (foldl EAp (EVar x) xs', concat dss)
        (x,xs@(_:_)) -> do
            (x',ds) <- g x
            (xs',dss) <- fmap unzip (mapM h xs)
            return (foldl EAp x' xs', concat (ds:dss))
    h e | isAtomic e = return (e,[])
    h (ELetRec ds e) = do
        (e',ds') <- h e
        return (e',ds' ++ ds)
    h e = do
        tick stats (toAtom "E.LetFloat.atomizeAp")
        u <- newUniq
        let n = toName Val (show modName,"a@" ++ show u)
            tv = tvr { tvrIdent = toId n, tvrType = infertype dataTable e }
        --C.putStrLn $ show n ++ " = " ++ pprint e
        return (EVar tv,[(tv,e)])
    isAtomic :: E -> Bool
    isAtomic EVar {}  = True
    isAtomic e | not atomizeTypes && sortTypeLike e = True
    isAtomic e = isFullyConst e

doCoalesce :: Stats -> (E,[E]) -> IO (E,[E])
doCoalesce stats (x,xs) = ans where
    ans = do
        (xs',dss) <- fmap unzip (mapM at xs)
        case x of
            ELetRec ds' (ELetRec ds'' x') -> do
                liftIO $ tick stats (toAtom "E.LetFloat.coalesce.fromLet")
                fromLet2 (concat $ ds'':ds':dss) (foldl EAp x' xs')
            ec@ECase { eCaseScrutinee = (ELetRec ds' x') }  -> do
                liftIO $ tick stats (toAtom "E.LetFloat.coalesce.fromCase")
                fromLet2 (concat $ ds':dss) (foldl EAp (ec { eCaseScrutinee = x' } ) xs')
            ELetRec ds' x' | not (List.null xs) -> do
                liftIO $ tick stats (toAtom "E.LetFloat.coalesce.fromAp")
                fromLet2 (concat $ ds':dss) (foldl EAp x' xs')
            ELetRec ds x' -> do
                fromLet2 (concat $ ds:dss) (foldl EAp x' xs')
            x -> fromLet2 (concat dss) (foldl EAp x xs')
    at (ELetRec ds e) = do
        liftIO $ tick stats (toAtom "E.LetFloat.coalesce.fromArg")
        return (e,ds)
    at e = return (e,[])
    --at' (t,(ELetRec ds e)) = do
    --    liftIO $ tick stats (toAtom "E.LetFloat.coalesce.fromLet2")
    --    return ((t,e),ds)
    at' e = return (e,[])
    fromLet2 ds e = do
        (ds',dss) <- fmap unzip (mapM at' ds)
        let ds'' = (concat $ ds':dss)
        r <- doLetRec stats ds''  e
        return $ fromAp r

fvBind (Left (_,fv)) = fv
fvBind (Right xs) = unions (snds xs)



floatInward ::
    Rules -- ^ rules to augment free variables of definitons
    -> E  -- ^ input term
    -> E  -- ^ output term
floatInward rules e = f e [] where
    augment fvs = fvs
    f ec@ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault =  d } xs = letRec p' $ ec { eCaseScrutinee = (f e pe), eCaseAlts = [ Alt l (f e pn) | Alt l e <- as | pn <- ps ], eCaseDefault = (fmap (flip f pd) d)}  where
        (p',_:pe:pd:ps) = sepByDropPoint (mconcat [freeVars l | Alt l _ <- as ]:freeVars e: tvrIdent b `delete` freeVars d :[freeVars a | a <- as ]) xs
    f (ELetRec ds e) xs = g (G.scc $  G.newGraph [ (d,bindingFreeVars x y) | d@(x,y) <- ds ] (tvrIdent . fst . fst) (idSetToList . snd) ) xs where
        g [] p' = f e p'
        g ((Left ((v,ev),fv)):xs) p = g xs (p0 ++ [Left ((v,ev'),bindingFreeVars v ev')] ++ p') where
            ev' = f ev pv
            (p',[p0,pv,_]) = sepByDropPoint [augment (frest xs), bindingFreeVars v ev, freeVars (tvrType v)] p
        g (Right bs:xs) p =  g xs (p0 ++ [Right [ let ev' = f ev pv in ((v,ev'),bindingFreeVars v ev') | ((v,ev),_) <- bs | pv <- ps ]] ++ p') where
            (p',_:p0:ps) = sepByDropPoint (freeVars (map (tvrType . fst . fst) bs) :augment (frest xs):snds bs) p
        frest xs = mconcat (freeVars e:map fvBind xs)
    f e@ELam {} xs = letRec unsafe_to_dup (foldr ELam (f b safe_to_dup) ls) where
        (unsafe_to_dup,safe_to_dup) = sepDupableBinds (freeVars ls) xs
        (b,ls) = fromLam e
    f e (Left ((v',ev),_):xs)
        | (EVar v,as) <- fromAp e, v == v', not (tvrIdent v' `member` (freeVars as :: IdSet))  = f (runIdentity $ app (ev,as) {- foldl EAp ev as -} ) xs
    f e xs = letRec xs e
    letRec [] e = e
    letRec xs e = f (G.scc $ G.newGraph (concatMap G.fromScc xs) (tvrIdent . fst . fst) (idSetToList . snd)) where
        f [] = e
        f (Left (te,_):rs) = eLetRec [te] $ f rs
        f (Right ds:rs) = eLetRec (fsts ds) $ f rs

type FVarSet = IdSet
type Binds = [Either ((TVr,E),FVarSet) [((TVr,E),FVarSet)]]

sepDupableBinds :: [Id] -> Binds -> (Binds,Binds)
sepDupableBinds fvs xs = partition ind xs where
    g = G.reachable (G.newGraph (concatMap G.fromScc xs) (tvrIdent . fst . fst) (idSetToList . snd)) (fvs `mappend` unsafe_ones)
    uso = map (tvrIdent . fst . fst) g
    unsafe_ones = concat [ map (tvrIdent . fst . fst) vs | vs <- map G.fromScc xs,any (not . isCheap) (map (snd . fst) vs)]
    ind x = any ( (`elem` uso) . tvrIdent . fst . fst ) (G.fromScc x)


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
    fvDecls (Left ((t,_),_)) = [tvrIdent t]
    fvDecls (Right ts) = [tvrIdent t | ((t,_),_) <- ts ]
    comb (a,b) (c,d) = (a ++ c, zipWith (++) b d)



coalesceLets :: Stats -> E -> IO E
coalesceLets stats e = liftM fst $ traverse travOptions { pruneRecord = varElim stats } f mempty mempty e where
    f n (x,xs) = do
        (x',xs') <- lift $ doCoalesce stats (x,xs)
        return $ foldl EAp x' xs'

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
        f n (t,e) imap = (tvrInfo_u (Info.insert n) t,g n e)
        g n e@ELam {} imap = foldr ELam (g n' b imap') ts' where
            (b,ts) = fromLam e
            n' = succ n
            ts' = map (tvrInfo_u (Info.insert n')) ts
            imap' = Map.fromList [ (tvrIdent t,n') | t <- ts] `Map.union` imap
        g n ec@ECase {} imap = runIdentity $ caseBodiesMapM (\e -> g' n e imap') ec { eCaseBind = m (eCaseBind ec), eCaseAlts = map ma (eCaseAlts ec) } where
            m t = tvrInfo_u (Info.insert n) t
            ma (Alt (LitCons n xs t)  b) = Alt (LitCons n (map m xs) t) b
            ma a = a
            imap' = Map.fromList [ (tvrIdent t,n) | t <- caseBinds ec] `Map.union` imap
        g n (ELetRec ds e) imap = dds (map G.fromScc $ decomposeDs ds) [] e imap where
            dds (ts:rs) nrs e imap = dds rs (ts':nrs) e imap' where
                n' = maximum (top_level:[ lup t | t <- fvs ])
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
    let imap = Map.fromList $ map (\x -> (x,top_level)) ([ tvrIdent t| (t,_) <-  programDs prog ] ++ idSetToList (progExternalNames prog))
    prog <- flip programMapDs prog (\ (t,e) -> do
        e' <- letBindAll (progDataTable prog) (progModule prog) e
        return $ tl (t,e') imap)


    let dofloat (ELetRec ds e) = do
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
        dtl (t,e) = do
            (e,fs) <- runWriterT (dofloat e)
            let (e',fs') = case e of
                    ELetRec ds e -> (e,ds++snds fs)
                    _ -> (e,snds fs)
                -- we imediatly float inward to clean up cruft and spurious outwards floatings
                (e'',fs'') = cDefs $ floatInward mempty (ELetRec fs' e')
                cDefs (ELetRec ds e) = (e',ds ++ ds') where
                    (e',ds') = cDefs e
                cDefs e = (e,[])
            flip mapM_ (fsts $ fs'') $ \t -> do
                mtick $ "LetFloat.Full-Lazy.top_level.{" ++ maybeShowName t
            u <- newUniq
            let (fs''',sm') = unzip [ ((n,sm e),(t,EVar n)) | (t,e) <- fs'', let n = nn t ]
                sm = substLet sm'
                nn tvr = tvr { tvrIdent = toId $ lfName u (progModule prog) Val (tvrIdent tvr) }
            return $ (t,sm e''):fs'''
    (cds,stats) <- runStatT (mapM dtl $ programDs prog)
    let nprog = programSetDs (concat cds) prog
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
    f (ELetRec ds e) = do
        ds' <- mapMSnd f ds
        e' <- g e
        return $ ELetRec ds' e'
    f ec@ECase {} = do
        ec' <- caseBodiesMapM g ec
        let mv = case eCaseScrutinee ec of
                EVar v -> subst (eCaseBind ec) (EVar v)
                _ -> id
            nd = fmap mv (eCaseDefault ec')
        scrut' <- g (eCaseScrutinee ec)
        return ec' { eCaseScrutinee = scrut', eCaseDefault = nd }
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



