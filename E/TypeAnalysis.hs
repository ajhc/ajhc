-- | examine all uses of types in a program to determine which ones are
-- actually needed in the method generation

module E.TypeAnalysis(typeAnalyze) where

import Control.Monad.Identity
import Data.Monoid
import Data.Typeable
import Data.FunctorM
import List(intersperse)
import qualified Data.Set as Set
import qualified Data.Map as Map

import CanType
import Doc.DocLike
import E.Annotate
import E.E hiding(isBottom)
import E.TypeCheck
import Fixer
import GenUtil
import Name
import qualified Info.Info as Info


type Typ = VMap Name

extractValMap :: [(TVr,E)] -> Map.Map Id [Value Typ]
extractValMap ds = Map.fromList [ (tvrIdent t,f e []) | (t,e) <- ds] where
    f (ELam tvr e) rs | sortStarLike (getType tvr) = f e (runIdentity (Info.lookup $ tvrInfo tvr):rs)
    f _ rs = reverse rs

typeAnalyze :: [(TVr,E)] -> IO [(TVr,E)]
typeAnalyze ds = do
    fixer <- newFixer
    let lambind _ nfo = do
            x <- newValue fixer ( bottom :: Typ)
            return $ Info.insert x nfo
        lamread _ nfo = do
            rv <- readValue (runIdentity $ Info.lookup nfo)
            return (Info.insert (rv :: Typ) $ Info.delete (undefined :: Value Typ) nfo)
        lamdel _ nfo = return (Info.delete (undefined :: Value Typ) nfo)
    ds <- annotateDs mempty lambind (\_ -> return) (\_ -> return) ds
    calcDs (extractValMap ds) ds
    findFixpoint fixer
    ds <- annotateDs mempty (\_ -> return) (\_ -> return) lamread ds
    ds <- annotateDs mempty lamdel (\_ -> return) (\_ -> return) ds
    return ds

calcDs :: Map.Map Id [Value Typ] -> [(TVr,E)] -> IO ()
calcDs env ds = mapM_ d ds >> mapM_ (calcE env) (snds ds) where
    d (t,e) | not (sortStarLike (getType t)) = return ()
    d (t,e) | Just v <- getValue e = do
        let Just t' = Info.lookup (tvrInfo t)
        t' `isSuperSetOf` v
    d (t, ELit (LitCons n xs _)) = do
        let Just t' = Info.lookup (tvrInfo t)
            v = vmapSingleton n
        t' `isSuperSetOf` (value v)
        xs' <- mapM getValue xs
        flip mapM_ (zip xs' [0.. ])  $ \ (v,i) -> do
            modifiedSuperSetOf t' v (vmapArgSingleton n i)
    d (t,e) | (EVar v,as) <- fromAp e = do
        let Just t' = Info.lookup (tvrInfo t)
            Just v' = Info.lookup (tvrInfo v)
        as' <- mapM getValue as
        dynamicRule v' $ \ v -> flip mapM_ (vmapHeads v) $ \ h -> do
            t' `isSuperSetOf` value (vmapSingleton h)
            flip mapM_ (zip as' [0.. ])  $ \ (a,i) -> do
                modifiedSuperSetOf t' a $ \ v -> vmapArgSingleton h i v
    d (t,e) = fail $ "calcDs: " ++ show (t,e)

calcAlt env v (Alt (LitCons n xs _) e) = do
    calcE env e
    flip mapM_ (zip [0..] xs) $ \ (i,t) -> do
        let Just t' = Info.lookup (tvrInfo t)
        modifiedSuperSetOf t' v (vmapArg n i)


calcE :: Map.Map Id [Value Typ] -> E -> IO ()
calcE env (ELetRec ds e) = calcDs nenv ds >> calcE nenv e where
    nenv = extractValMap ds `Map.union` env
calcE env e | (e',(_:_)) <- fromLam e = calcE env e'
calcE env ec@ECase {} | sortStarLike (getType $ eCaseScrutinee ec) = do
    calcE env (eCaseScrutinee ec)
    fmapM_ (calcE env) (eCaseDefault ec)
    v <- getValue (eCaseScrutinee ec)
    mapM_ (calcAlt env v) (eCaseAlts ec)
    calcScrut (eCaseScrutinee ec)
calcE env ec@ECase {} = do
    calcE env (eCaseScrutinee ec)
    mapM_ (calcE env) (caseBodies ec)
calcE _ ELit {} = return ()
calcE _ EPrim {} = return ()
calcE _ EError {} = return ()
calcE _ ESort {} = return ()
calcE _ Unknown = return ()
calcE env e | (EVar v,as@(_:_)) <- fromAp e, Just ts <- Map.lookup (tvrIdent v) env = do
    flip mapM_ (zip as ts) $ \ (a,t) -> do
        when (sortStarLike (getType a)) $ do
            a' <- getValue a
            t `isSuperSetOf` a'
calcE _ EVar {} = return ()
calcE _ EAp {} = return ()
calcE _ e = fail $ "odd calcE: " ++ show e

calcScrut _ = return ()

getValue (EVar v)
    | Just x <- Info.lookup (tvrInfo v) = return x
    | otherwise = fail $ "getValue: no varinfo: " ++ show v
getValue e | Just c <- typConstant e = return $ value c
getValue e = fail $ "getValue: " ++ show e

typConstant :: Monad m => E -> m Typ
typConstant (ELit (LitCons n xs _)) = do
    xs' <- mapM typConstant xs
    return $ vmapValue n xs'
typConstant e = fail $ "typConstant: " ++ show e


-- VMap general structure

data VMap n = VMap (Map.Map (n,Int) (VMap n)) (Set.Set n)
    deriving(Typeable)

vmapSingleton n = VMap Map.empty (Set.singleton n)

vmapArgSingleton n i v
    | isBottom v = bottom
    | otherwise = VMap (Map.singleton (n,i) v) Set.empty

vmapArg n i (VMap map _) = case Map.lookup (n,i) map of
    Just x -> x
    Nothing -> bottom

vmapValue :: Ord n => n -> [VMap n] -> VMap n
vmapValue n xs = pruneVMap $ VMap (Map.fromAscList (zip (zip (repeat n) [0..]) xs)) (Set.singleton n)

vmapHeads (VMap _ set) = Set.toList set
vmapJustHeads (VMap _ set) = VMap Map.empty set

pruneVMap (VMap map set) = VMap map' set where
    map' = Map.filter f map
    f vs = not $ isBottom vs

instance (Ord n,Show n) => Show (VMap n) where
    showsPrec _ (VMap n s) = braces (hcat (intersperse (char ',') $ (map f $ snub $ fsts  (Map.keys n) ++ Set.toList s) )) where
        f a = (if a `Set.member` s then tshow a else char '#' <> tshow a) <> tshow (g a)
        g a = sortUnder fst [ (i,v) | ((a',i),v) <- Map.toList n, a' == a ]

instance Ord n => Fixable (VMap n) where
    bottom = VMap Map.empty Set.empty
    isBottom (VMap m s) = Map.null m && Set.null s
    lub (VMap as ns) (VMap as' ns') = pruneVMap $ VMap (Map.unionWith lub as as') (Set.union ns ns')
    minus (VMap n1 w1) (VMap n2 w2) = pruneVMap $ VMap (Map.fromAscList $ [
            case Map.lookup (a,i) n2 of
                Just v' ->  ((a,i),v `minus` v')
                Nothing ->  ((a,i),v)
        | ((a,i),v) <- Map.toAscList n1 ] ) (w1 Set.\\ w2)

instance Ord n => Monoid (VMap n) where
    mempty = bottom
    mappend = lub


instance Ord n => Fixable (Set.Set n)  where
    bottom = Set.empty
    isBottom = Set.null
    lub a b = Set.union a b
    minus a b = a Set.\\ b


instance Fixable Bool where
    bottom = False
    isBottom x = x == False
    lub a b = a || b
    minus True False = True
    minus False True = False
    minus True True = False
    minus False False = False

