
module E.Traverse(
    emapE_,
    emapE,
    emapE',
    emapEG,
    emapEGH,
    eSize,
    renameE,
    scopeCheck,
    runRename
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Monoid
import qualified Data.Traversable as T

import E.E
import E.FreeVars(caseUpdate)
import E.Type
import Name.Id
import Util.Gen
import Util.HasSize
import Util.NameMonad
import Util.SetLike as S

-- Generic traversal routines rock.

newtype MInt = MInt Int

instance Monoid MInt where
    mempty = MInt 0
    mappend (MInt a) (MInt b) = a `seq` b `seq` MInt (a + b)


runRename :: IdSet -> E -> (E,IdSet)
runRename set e = renameE set mempty e


emapE_ :: Monad m => (E -> m a) -> E -> m ()
emapE_ f e = emapEG f' f' e >> return () where
    f' e = f e >> return e
emapE f = emapEG f f
emapE' f = emapEG f return

emapEG f g e = emapEGH f g g e

emapEGH f g h e = z e where
    z (EAp aa ab) = do aa <- f aa;ab <- f ab; return $ EAp aa ab
    z (ELam aa ab) = do aa <- mapmTvr g aa; ab <- f ab; return $ ELam aa ab
    z (EPi aa ab) = do aa <- mapmTvr f aa; ab <- f ab; return $ EPi aa ab
    z (EVar aa) = do aa <- mapmTvr h aa; return $ EVar aa
    z (Unknown) = do return $ Unknown
    z (ESort aa) = do return $ ESort aa
    z (ELit lc@LitCons { litArgs = es, litType = t }) = do t' <- g t; es' <- mapM f es; return $ ELit lc { litArgs = es', litType = t' }
    z (ELit aa) = do aa <- T.mapM g aa; return $ ELit aa
    z ELetRec { eDefs = aa, eBody = ab } = do aa <- mapM (\x -> do x <- (do (aa,ab) <- return x; aa <- mapmTvr g aa;ab <- f ab;return (aa,ab)); return x) aa;ab <- f ab; return $ ELetRec aa ab
    z ec@ECase {} = do
        e' <- f $ eCaseScrutinee ec
        b' <- T.mapM g (eCaseBind ec)
        as' <- mapM mapmAlt (eCaseAlts ec)
        d' <- T.mapM f (eCaseDefault ec)
        t' <- g (eCaseType ec)
        return $ caseUpdate ec { eCaseScrutinee =e', eCaseBind = b', eCaseAlts = as', eCaseDefault = d', eCaseType = t'}
    --    aa ab) = do aa <- f aa;ab <- mapM (\(x,y) -> do x <- T.mapM f x; y <- f y; return (x,y)) ab; return $ ECase aa ab
    z (EPrim aa ab ac) = do ab <- mapM f ab;ac <- g ac; return $ EPrim aa ab ac
    z (EError aa ab) = do ab <- g ab; return $ EError aa ab
    mapmTvr = T.mapM
    mapmAlt (Alt lc@LitCons {  litArgs = xs, litType = t } e) = do
        e' <- f e
        xs' <- mapM (T.mapM g) xs
        t' <- g t
        return $ Alt lc { litArgs = xs', litType = t' } e'
    mapmAlt (Alt l e) = do
        e' <- f e
        l' <- T.mapM g l
        return (Alt l' e')



instance HasSize E where
    size = eSize

eSize :: E -> Int
eSize e = n where
    (_, MInt n) = runWriter (f e)
    f e@ELit {} = tell (MInt 1) >> return e
    f e@EPrim {} = tell (MInt 1) >> return e
    f e@EError {} = tell (MInt 1) >> return e
    f e = tell (MInt 1) >> emapE' f e


renameE :: IdSet -> IdMap E -> E -> (E,IdSet)
renameE initSet initMap e = runReader (runIdNameT' $ addBoundNamesIdMap initMap >> addBoundNamesIdSet initSet >> f e) initMap  where
    f,f' :: E -> IdNameT (Reader (IdMap E)) E
    f' e = f e
    f  (EAp a b) = return EAp `ap` f a `ap` f b
    f  (ELit lc@LitCons { litArgs = xs, litType = t }) = do
        xs' <- mapM f xs
        t' <- f' t
        return $ ELit lc { litArgs = xs', litType = t' }
    f (ELit (LitInt n t)) = do
        t' <- f' t
        return (ELit (LitInt n t'))
    f (EError x t) = return (EError x) `ap` f' t
    f (EPrim n es t) = do
        es' <- mapM f es
        t' <- f' t
        return $ EPrim n es' t'
    f (ELam tvr e) = lp f' ELam tvr e
    f (EPi tvr e) = lp f EPi tvr e
    f  e@(EVar TVr { tvrIdent = n }) = do
        im <- lift ask
        case mlookup n im of
            Just n' -> do return n'
            Nothing -> return e
    f x@(ESort {}) = return x
    f Unknown = return Unknown
    f ec@ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault = d } = do
        e' <- f e
        t' <- f' (eCaseType ec)
        addNames $ map tvrIdent (caseBinds ec)
        (ob,b') <- ntvr False f' b
        localSubst ob $ do
            as' <- mapM da as
            d' <- T.mapM f d
            return $ caseUpdate ec { eCaseScrutinee = e', eCaseType = t', eCaseBind = b', eCaseAlts = as', eCaseDefault = d' }
    f ELetRec { eDefs = ds, eBody = e } = do
        addNames (map (tvrIdent . fst) ds)
        ds' <- mapM ( ntvr False f' . fst) ds
        localSubst (mconcat $ fsts ds') $ do
            es <- mapM f (snds ds)
            e' <- f e
            return (ELetRec (zip (snds ds') es) e')
    --f e = error $ "renameE.f: " ++ show e
    da :: Alt E -> IdNameT (Reader (IdMap E)) (Alt E)
    da (Alt lc@LitCons { litName = n, litArgs = xs, litType = t } l) = do
        t' <- f' t
        xs' <-  mapM (ntvr False f') xs
        localSubst (mconcat [ x | (x,_) <- xs']) $ do
            l' <- f l
            return (Alt lc { litArgs = snds xs', litType = t' } l')
    da (Alt (LitInt n t) l) = do
        t' <- f' t
        l' <- f l
        return (Alt (LitInt n t') l')
    localSubst :: (IdMap E) -> IdNameT (Reader (IdMap E)) a  -> IdNameT (Reader (IdMap E)) a
    localSubst ex action = do local (ex `mappend`) action
    ntvr _ fg tv@TVr { tvrIdent = 0, tvrType = t} = do
        t' <- fg t
        return (mempty,tv { tvrType = t'})
    ntvr ralways fg tv@(TVr { tvrIdent = n, tvrType = t}) = do
        n' <- if n > 0 && (not ralways || odd n) then uniqueName  n else newName
        t' <- fg t
        let tv' = tv { tvrIdent = n', tvrType = t' }
        return (msingleton n (EVar tv'),tv')
    lp fg elam tv e = do
        (n,tv') <- ntvr True fg tv
        e' <- localSubst n (f e)
        return $ elam tv' e'


scopeCheck :: Monad m => Bool -> IdMap TVr -> E -> m ()
scopeCheck checkFvs initMap e = runReaderT (f e) initMap  where
    f (ELam tvr e) = f (tvrType tvr) >> local (minsert (tvrIdent tvr) tvr) (f e)
    f (EPi tvr e) = f (tvrType tvr) >> local (minsert (tvrIdent tvr) tvr) (f e)
    f (EVar t) = do
        m <- ask
        case mlookup (tvrIdent t) m of
            Nothing | checkFvs -> fail $ "scopeCheck: found variable not in scope " ++ tvrShowName t
            Just t' | tvrType t /= tvrType t' -> fail $ "scopeCheck: found variable whose type does not match " ++ tvrShowName t
            _ -> return ()
    f ec@ECase { eCaseBind = b } = do
        f (eCaseScrutinee ec)
        f (eCaseType ec)
        f (tvrType b)
        local (minsert (tvrIdent b) b) $ mapM_ doAlt (eCaseAlts ec)
    f ELetRec { eDefs = ds, eBody = e } = do
        mapM_ (f . tvrType . fst) ds
        local (fromList [ (tvrIdent t,t) | (t,_) <- ds ] `mappend`) (f e)
    f e = emapE_ f e
    doAlt (Alt LitCons { litArgs = xs, litType = t } e) = do
        f t >> local (fromList [ (tvrIdent t,t) | t <- xs] `mappend`) (f e)
    doAlt (Alt (LitInt _ t) e) = f t >> f e



