module E.Demand(
    Demand(..),
    DemandSignature(..),
    DemandType(..),
    analyzeProgram,
    lazySig,
    lazy,
    lazyType
    ) where


import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Monoid
import Data.Maybe
import Data.Typeable
import qualified Data.Map as Map

import Binary
import DataConstructors
import Doc.DocLike
import Doc.PPrint
import E.E
import E.Inline
import E.Program
import GenUtil
import Name.Id
import qualified Info.Info as Info
import Util.HasSize
import Util.SetLike

data Demand =
    Bottom             -- always diverges
    | L SubDemand      -- lazy
    | S SubDemand      -- strict
    | Error SubDemand  -- diverges, might use arguments
    | Absent           -- Not used
    deriving(Eq,Ord,Typeable)
        {-! derive: GhcBinary !-}

instance Show Demand where
    showsPrec _ Bottom = ("_|_" ++)
    showsPrec _ Absent = ('A':)
    showsPrec _ (L None) = ('L':)
    showsPrec _ (L (Product ds)) = showString "L(" . foldr (.) id (map shows ds) . showString ")"
    showsPrec _ (S None) = ('S':)
    showsPrec _ (S (Product ds)) = showString "S(" . foldr (.) id (map shows ds) . showString ")"
    showsPrec _ (Error None) = showString "Err"
    showsPrec _ (Error (Product ds)) = showString "Err(" . foldr (.) id (map shows ds) . showString ")"

instance DocLike d => PPrint d Demand where
    pprint demand = tshow demand

data SubDemand = None | Product [Demand]
    deriving(Eq,Ord,Typeable)
        {-! derive: GhcBinary !-}

data DemandSignature = DemandSignature !Int DemandType
    deriving(Eq,Ord,Typeable)
        {-! derive: GhcBinary !-}
data DemandType = (:=>) DemandEnv [Demand]
    deriving(Eq,Ord,Typeable)
        {-! derive: GhcBinary !-}
data DemandEnv = DemandEnv (Map.Map TVr Demand) Demand
    deriving(Eq,Ord,Typeable)
        {-! derive: GhcBinary !-}

instance Show DemandType where
    showsPrec _ (DemandEnv e Absent :=> d) | isEmpty e = shows d
    showsPrec _ (env :=> ds) = shows env . showString " :=> " .  shows ds

instance Show DemandEnv where
    showsPrec _ (DemandEnv m Absent) = pprint m
    showsPrec _ (DemandEnv _ Bottom) = showString "_|_"


instance Show DemandSignature where
    showsPrec _ (DemandSignature n dt) = showString "<" . shows n . showString "," . shows dt . showString ">"

idGlb = Absent

absType = (DemandEnv mempty idGlb) :=> []
botType = (DemandEnv mempty Bottom) :=> []

lazyType = (DemandEnv mempty lazy) :=> []
lazySig = DemandSignature 0 lazyType

class Lattice a where
    glb :: a -> a -> a
    lub :: a -> a -> a



-- Sp [L .. L] = S
-- Sp [.. _|_ ..] = _|_

sp [] = S None
sp s = sp' True s where
    sp' True [] = S None
    sp' False [] = S (Product s)
    sp' allLazy (L _:rs) = sp' allLazy rs
    sp' _ (Bottom:_) = Error (Product s)
    sp' _ (_:rs) = sp' False rs


instance Lattice DemandType where
    lub (env :=> ts) (env' :=> ts') = (env `lub` env') :=> zipWith lub ts ts'
    glb (env :=> ts) (env' :=> ts') = (env `glb` env') :=> zipWith glb ts ts'

lazy = L None
strict = S None
err = Error None

comb _ None None = None
comb f None (Product xs) = Product $ zipWith f (repeat lazy) xs
comb f (Product xs) None = Product $ zipWith f xs (repeat lazy)
comb f (Product xs) (Product ys) = Product $ zipWith f xs ys


instance Lattice Demand where
    lub Bottom s = s
    lub s Bottom = s
    lub Absent Absent = Absent
    lub (S x) Absent = L x
    lub Absent (S x) = L x
    lub Absent sa = lazy
    lub sa Absent = lazy

    lub (S x) (S y) = S (comb lub x y)
    lub (L x) (L y) = L (comb lub x y)
    lub (Error x) (Error y) = Error (comb lub x y)

    lub (S x) (L y) = L (comb lub x y)
    lub (L x) (S y) = L (comb lub x y)

    lub (S x) (Error y) = S (comb lub x y)
    lub (Error x) (S y) = S (comb lub x y)

    lub (L x) (Error y) = lazy
    lub (Error x) (L y) = lazy


    glb Bottom Bottom = Bottom
    glb Absent sa = sa
    glb sa Absent = sa

    glb Bottom _ = err
    glb _ Bottom = err

    glb (S x) (S y) = S (comb glb x y)
    glb (L x) (L y) = L (comb glb x y)
    glb (Error x) (Error y) = Error (comb glb x y)

    glb (S _) (Error _) = err
    glb (Error _) (S _) = err

    glb (S x) (L y) = S (comb glb x y)
    glb (L x) (S y) = S (comb glb x y)

    glb (L _) (Error _) = err
    glb (Error _) (L _) = err



lenv e (DemandEnv m r) = case Map.lookup e m of
    Nothing -> r
    Just x -> x

demandEnvSingleton :: TVr -> Demand -> DemandEnv
demandEnvSingleton _ Absent = DemandEnv mempty idGlb
demandEnvSingleton t d = DemandEnv (Map.singleton t d) idGlb

demandEnvMinus :: DemandEnv -> TVr -> DemandEnv
demandEnvMinus (DemandEnv m r) x = DemandEnv (Map.delete x m) r

instance Lattice DemandEnv where
    lub d1@(DemandEnv m1 r1) d2@(DemandEnv m2 r2) = DemandEnv m (r1 `lub` r2) where
        m = Map.fromList [ (x,lenv x d1 `lub` lenv x d2) | x <- Map.keys m1 ++ Map.keys m2]
    glb d1@(DemandEnv m1 r1) d2@(DemandEnv m2 r2) = DemandEnv m (r1 `glb` r2) where
        m = Map.fromList [ (x,lenv x d1 `glb` lenv x d2) | x <- Map.keys m1 ++ Map.keys m2]


newtype IM a = IM (Reader (IdMap DemandSignature,DataTable) a)
    deriving(Monad,Functor,MonadReader (IdMap DemandSignature,DataTable))

getEnv :: IM (IdMap DemandSignature)
getEnv = asks fst

extEnv t e = local (\ (env,dt) -> (minsert (tvrIdent t) e env,dt))
extEnvs ts = local  (\ (env,dt) -> (mappend (fromList [ (tvrIdent t,s) |  (t,s) <- ts]) env,dt))


instance DataTableMonad IM where
    getDataTable = asks snd

runIM :: Monad m => IM a -> DataTable ->  m a
runIM (IM im) dt = return $ runReader im (mempty,dt)

determineDemandType :: TVr -> Demand -> IM DemandType
determineDemandType tvr demand = do
    env <- getEnv
    case mlookup (tvrIdent tvr) env `mplus` Info.lookup (tvrInfo tvr) of
        Nothing -> return absType
        Just (DemandSignature n dt) -> f n demand where
            f 0 _ = return dt
            f n (S (Product [s])) = f (n - 1) s
            f _ _ = return absType

splitSigma [] = (lazy,[])
splitSigma (x:xs) = (x,xs)

analyze :: E -> Demand -> IM (E,DemandType)
analyze e Absent = return (e,absType)
analyze (EVar v) s = do
    phi :=> sigma <- determineDemandType v s
    return (EVar v,(phi `glb` (demandEnvSingleton v s)) :=> sigma)
analyze (EAp e1 e2) s = do
    (e1',phi1 :=> sigma1') <- analyze e1 (sp [s])
    let (sa,sigma1) = splitSigma sigma1'
    (e2',phi2 :=> sigma2) <- analyze e2 sa
    return $ (EAp e1' e2',(phi1 `glb` phi2) :=> sigma1)
analyze el@(ELit LitCons { litName = h, litArgs = ts@(_:_) }) (S (Product ss)) | length ss == length ts = do
    dataTable <- getDataTable
    case getSiblings dataTable h of
        Just [_] -> do  -- product type
            envs <- flip mapM (zip ts ss) $ \(a,s) -> do
                (_,env :=> _) <- analyze a s
                return env
            return (el,foldr1 glb envs :=> [])
        _ -> return (el,absType)

analyze (ELit lc@LitCons { litArgs = ts }) _s = do
    rts <- mapM (\e -> analyze e lazy) ts
    return (ELit lc { litArgs = fsts rts }, foldr glb absType (snds rts))
analyze (EPrim ap ts pt) _s = do
    rts <- mapM (\e -> analyze e lazy) ts
    return (EPrim ap (fsts rts) pt, foldr glb absType (snds rts))
analyze (EPi tvr@TVr { tvrType = t1 } t2)  _s = do
    (t1',dt1) <- analyze t1 lazy
    (t2',dt2) <- analyze t2 lazy
    return (EPi tvr { tvrType = t1' } t2',dt1 `glb` dt2)

analyze (ELam x e) (S (Product [s])) = do
    (e',phi :=> sigma) <- analyze e s
    let sx = lenv x phi
    return (ELam (tvrInfo_u (Info.insert sx) x) e',demandEnvMinus phi x :=> (sx:sigma))
analyze (ELam x e) (S None) = analyze (ELam x e) (S (Product [lazy]))  -- simply to ensure binder is annotated
analyze e@EError {} _ = return (e,botType)
analyze ec@ECase { eCaseAlts = [Alt lc@(LitCons h ts _) alt], eCaseDefault = Nothing } s = do
    dataTable <- getDataTable
    case getSiblings dataTable h of
        Just [_] -> do  -- product type
            (alt',enva :=> siga) <- analyze alt s
            (e',enve :=> []) <- analyze (eCaseScrutinee ec) (sp [ lenv t enva | t <- ts])
            let nenv = foldr denvDelete (glb enva enve) ts
            return (ec { eCaseScrutinee = e', eCaseAlts = [Alt lc alt'] }, nenv :=> siga)
        _ -> analyzeCase ec s
analyze ec@ECase {} s = analyzeCase ec s
analyze (ELetRec ds b) s = f (decomposeDs ds) [] where
    f [] ds' = do
        (b',phi :=> sig) <- analyze b s
        let g (t,e) = (tvrInfo_u (Info.insert (lenv t phi)) t,e)
        return (ELetRec (map g ds') b', foldr denvDelete phi (fsts ds) :=> sig)
    f (Left (t,e):rs) fs = do
        (ne,ds) <- topAnalyze e
        extEnv t ds $ do
            f rs ((tvrInfo_u (Info.insert ds) t,ne):fs)
    f (Right rg:rs) fs = do
        rg' <- extEnvs [ (t,ds)| (t,_) <- rg, let il@(~(Just ds)) = Info.lookup (tvrInfo t), isJust il] $ do
            flip mapM rg $ \ (t,e) -> do
                (ne,ds) <- topAnalyze e
                return ((tvrInfo_u (Info.insert ds) t,ne),Just ds == Info.lookup (tvrInfo t))
        let (rg'',cs) = unzip rg'
        if and cs then
            extEnvs [ (t,ds)| (t,_) <- rg'', let (Just ds) = Info.lookup (tvrInfo t)] $ do
                f rs (rg'' ++ fs)
          else f (Right rg'':rs) fs
analyze e _ = return (e,absType)


analyzeCase ec@ECase {} s = do
    (ec',dts) <- runWriterT $ flip caseBodiesMapM ec $ \e -> do
        (ne,dt) <- lift $ analyze e s
        tell (dt:)
        return ne
    (ecs,env :=> _) <- analyze (eCaseScrutinee ec') (S None)
    let enva :=> siga =  foldr1 lub (dts [])
    let nenv = foldr denvDelete (glb enva env) (caseBinds ec')
    return (ec' {eCaseScrutinee = ecs},nenv :=> siga)

denvDelete x (DemandEnv m r) = DemandEnv (Map.delete x m) r



topAnalyze :: E -> IM (E,DemandSignature)
topAnalyze e = clam e (S None) 0 where
    clam (ELam _ x) s n = clam x (sp [s]) (n + 1)
    clam _ s n = do
        (e,dt) <- analyze e s
        return (e,DemandSignature n dt)

fixupDemandSignature (DemandSignature n (DemandEnv _ r :=> dt)) = DemandSignature n (DemandEnv mempty r :=> dt)

{-# NOINLINE analyzeProgram #-}
analyzeProgram prog = do
    putStrLn "Beginning Demand analysis"
    ds' <- flip mapM (programDs prog) $ \ (t,e) -> case (runIM (topAnalyze e) (progDataTable prog)) of
        Left err -> do
            putStrLn $ "strictness error :" ++ pprint t ++ "\n" ++ err
            return (t,e)
        Right (ne,s) -> do
            let s' = fixupDemandSignature s
            putStrLn $ "strictness: " ++ pprint t ++ "\n" ++ show s'
            return (tvrInfo_u (Info.insert s') t,ne)
    putStrLn "Ending Demand analysis"
    return $ programSetDs ds' prog

--    flip mapM_ (programDs prog) $ \ (t,e) -> case (runIM (infer e)) of
--        Left err -> putStrLn $ "strictness error :" ++ pprint t ++ "\n" ++ err
--        Right (c,(ty,_)) -> do
--            putStrLn $ "strictnes " ++ pprint t
--            print c
--            let cc (TAnot l TAtomic) = strict `islte` l
--                cc (TAnot _ (_ `TFun` b)) = cc b
--            print (fmap fn ty)
--            putStrLn "solving:"
--            processConstraints True c
--    return ()




