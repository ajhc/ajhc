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

data Typ = L (Map.Map Name [Typ]) (Set.Set Name)
    deriving(Show,Typeable,Eq,Ord)


instance Fixable Typ where
    bottom = L Map.empty Set.empty
    isBottom (L map set) = Set.null set && Map.null map
    lub (L as ns) (L as' ns') = pruneTyp $ L (Map.unionWith (zipWith lub) as as') (Set.union ns ns')
    minus (L n1 w1) (L n2 w2) = pruneTyp $ L (Map.fromList $ [
            case Map.lookup n n2 of
                Just vs ->  (n,[ a `minus` v | v <- vs | a <- as ])
                Nothing ->  (n,as)
        | (n,as) <- Map.toList n1 ] ) (w1 Set.\\ w2)

pruneTyp (L map set) = L map' set where
    map' = Map.filter f map
    f vs = any (not . isBottom) vs

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
    ds <- annotateDs mempty lambind lambind lambind ds
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
        t' `isSuperSetOf` (value $ L mempty (Set.singleton n))
        xs' <- mapM getValue xs
        flip mapM_ [0.. length xs' - 1]  $ \i -> do
            modifiedSuperSetOf t' (xs' !! i) $ \v ->
                L (Map.singleton n [ if x == i then v else bottom | x <- [0 .. length xs' - 1]]) Set.empty
                {-
    d (t,e) | (EVar v,as) <- fromAp e = do
        let Just t' = Info.lookup (tvrInfo t)
            Just v' = Info.lookup (tvrInfo v)
        as' <- mapM getValue as
        dynamicRule v' $ \ (L mp st) -> do
            flip mapM_ (Set.toList st) $ \n -> do
                undefined
        flip mapM_ [0.. length xs' - 1]  $ \i -> do
            modifiedSuperSetOf t' (xs' !! i) $ \v ->
                L (Map.singleton n [ if x == i then v else bottom | x <- [0 .. length xs' - 1]]) Set.empty

-}

    d (t,e) = fail $ "calcDs: " ++ show (t,e)

calcAlt env v (Alt (LitCons n xs _) e) = do
    flip mapM_ (zip [0..] xs) $ \ (i,t) -> do
        let Just t' = Info.lookup (tvrInfo t)
        modifiedSuperSetOf t' v (\ (L map _) -> case Map.lookup n map of
            Just xs -> xs !! i
            Nothing -> bottom
            )




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
calcE _ EAp {} = return ()
calcE env e | (EVar v,as@(_:_)) <- fromAp e, Just ts <- Map.lookup (tvrIdent v) env = do
    flip mapM_ (zip as ts) $ \ (a,t) -> do
        when (sortStarLike (getType a)) $ do
            a' <- getValue a
            t `isSuperSetOf` a'
calcE _ EVar {} = return ()
calcE _ e = fail $ "odd calcE: " ++ show e

calcScrut _ = return ()

getValue (EVar v)
    | Just x <- Info.lookup (tvrInfo v) = return x
    | otherwise = fail $ "getValue: no varinfo: " ++ show v
getValue e | Just c <- typConstant e = return $ value c
getValue e = fail $ "getValue: " ++ show e

typConstant (ELit (LitCons n xs _)) = do
    xs' <- mapM typConstant xs
    return $  L (Map.singleton n xs') (Set.singleton n)
typConstant e = fail $ "typConstant: " ++ show e


-- VMap general structure

data VMap n = VMap (Map.Map (n,Int) (VMap n)) (Set.Set n)
    deriving(Typeable)

vmapSingleton n = VMap Map.empty (Set.singleton n)

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

