module E.Demand(
    analyzeProgram,
    Demand(..),
    DemandType(..),
    DemandSignature(..)
    ) where


import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Monoid
import Data.Maybe
import Data.Typeable
import qualified Data.Map as Map

import Doc.PPrint
import Doc.DocLike
import E.Inline
import E.E
import GenUtil
import DataConstructors
import qualified Info.Info as Info
import E.Program
import Name.Id
import Util.HasSize
import Util.SetLike

data Demand =
    Bottom    -- hyperstrict
    | L       -- lazy
    | S SubDemand      -- strict
    deriving(Eq,Ord,Typeable)

instance Show Demand where
    showsPrec _ Bottom = ("_|_" ++)
    showsPrec _ L = ('L':)
    showsPrec _ (S None) = ('S':)
    showsPrec _ (S (Product ds)) = showString "S(" . foldr (.) id (map shows ds) . showString ")"

instance DocLike d => PPrint d Demand where
    pprint demand = tshow demand

data SubDemand = None | Product [Demand]
    deriving(Eq,Ord,Typeable)

data DemandSignature = DemandSignature !Int DemandType
    deriving(Eq,Ord,Typeable)
data DemandType = DemandEnv :=> [Demand]
    deriving(Eq,Ord,Typeable)
data DemandEnv = DemandEnv (Map.Map TVr Demand) Demand
    deriving(Eq,Ord,Typeable)

instance Show DemandType where
    showsPrec _ (DemandEnv e L :=> d) | isEmpty e = shows d
    showsPrec _ (env :=> ds) = shows env . showString " :=> " .  shows ds

instance Show DemandEnv where
    showsPrec _ (DemandEnv m L) = pprint m
    showsPrec _ (DemandEnv _ Bottom) = showString "_|_"


instance Show DemandSignature where
    showsPrec _ (DemandSignature n dt) = showString "<" . shows n . showString "," . shows dt . showString ">"

idGlb = L

absType = (DemandEnv mempty idGlb) :=> []
botType = (DemandEnv mempty Bottom) :=> []

class Lattice a where
    glb :: a -> a -> a
    lub :: a -> a -> a



-- Sp [L .. L] = S
-- Sp [.. _|_ ..] = _|_

sp s = sp' True s where
    sp' True [] = S None
    sp' False [] = S (Product s)
    sp' allLazy (L:rs) = sp' allLazy rs
    sp' _ (Bottom:_) = Bottom
    sp' _ (_:rs) = sp' False rs


instance Lattice DemandType where
    lub (env :=> ts) (env' :=> ts') = (env `lub` env') :=> zipWith lub ts ts'
    glb (env :=> ts) (env' :=> ts') = (env `glb` env') :=> zipWith glb ts ts'

instance Lattice Demand where
    lub Bottom s = s
    lub s Bottom = s
    lub L _  = L
    lub _ L  = L
    lub (S (Product xs)) (S (Product ys)) | length xs == length ys = sp (zipWith lub xs ys)
    lub (S _) (S _) = S None


    glb Bottom _ = Bottom
    glb _ Bottom = Bottom
    glb L s = s
    glb s L = s
    glb (S None) (S None) = S None
    glb s1 s2 = sp (zipWith glb (sargs s1) (sargs s2))

sargs (S None) = repeat L
sargs (S (Product xs)) = xs

lenv e (DemandEnv m r) = case Map.lookup e m of
    Nothing -> r
    Just x -> x

demandEnvSingleton :: TVr -> Demand -> DemandEnv
demandEnvSingleton _ L = DemandEnv mempty idGlb
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

splitSigma [] = (L,[])
splitSigma (x:xs) = (x,xs)

analyze :: E -> Demand -> IM (E,DemandType)
analyze e L = return (e,absType)
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

analyze (ELam x e) (S (Product [s])) = do
    (e',phi :=> sigma) <- analyze e s
    let sx = lenv x phi
    return (ELam (tvrInfo_u (Info.insert sx) x) e',demandEnvMinus phi x :=> (sx:sigma))
analyze (ELam x e) (S None) = analyze (ELam x e) (S (Product [L]))  -- simply to ensure binder is annotated
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




