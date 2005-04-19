{------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)

        Module:                 Env

        Description:            A generic environment that supports mappings 
                                from names to values.

        Primary Authors:        Bernie Pope

        Notes:                  See the file License for license information
                                
                                Based on FiniteMaps

-------------------------------------------------------------------------------}

module FrontEnd.Env (Env, 
            emptyEnv, 
            unitEnv, 
            lookupEnv,
            addToEnv,
            joinEnv, 
            joinListEnvs, 
            listToEnv,
            envToList,
            getNamesFromEnv,
            showEnv,
            pprintEnv,
            addToCombFM,
            mapEnv,
            zeroFM,
            joinFM,lookupDftFM,
            toListFM,
            -- use these plus monoid routines when possible
            fromList,
            single,
            FrontEnd.Env.find,
            toList
           ) where


import Data.FiniteMap 
import HsSyn   
import PPrint  
import Utils
import Char
import List
import Data.Monoid


--------------------------------------------------------------------------------

{-# INLINE joinFM #-}
{-# INLINE emptyEnv #-}
{-# INLINE zeroFM #-}
{-# INLINE addToCombFM #-}
{-# INLINE lookupDftFM #-}
zeroFM = emptyFM
joinFM x y = plusFM y x
toListFM  = fmToList
addToCombFM c k v m = addToFM_C (flip c) m k v 
lookupDftFM x = lookupWithDefaultFM x
--addToFM x y = Data.FiniteMap.addToFM y x

--instance (Show a, Show b) => Show (FiniteMap a b) where
--    show fm = show (fmToList fm)

type Env a = FiniteMap HsName a 

instance Ord a => Monoid (FiniteMap a b) where
    mempty = emptyFM
    mappend = plusFM


emptyEnv :: Env a
emptyEnv = zeroFM


single k v = unitFM k v

unitEnv :: (HsName, a) -> Env a 
unitEnv (name, val) = unitFM name val

lookupEnv :: HsName -> Env a -> Maybe a 
lookupEnv name env
   = lookupFM env name


find :: HsName -> Env a -> a 
find name env = case lookupFM env name of
    Just x -> x
    Nothing -> error $ "Env.find: " ++ fromHsName name 


addToEnv :: (HsName, a) -> Env a -> Env a
addToEnv (name, val) env = addToFM env name val 

-- this might be expensive!
joinEnv :: Env a -> Env a -> Env a 
joinEnv env1 env2 
   = joinFM env1 env2

joinListEnvs :: [Env a] -> Env a 
joinListEnvs = foldr joinEnv emptyEnv

listToEnv :: [(HsName, a)] -> Env a 
listToEnv = foldr addToEnv emptyEnv  

fromList = listToEnv
toList = toListFM 

envToList :: Env a -> [(HsName, a)]
envToList env
   = toListFM env 

-- just get all the names out of the Env (added by Bryn)
getNamesFromEnv :: Env a -> [HsName]
getNamesFromEnv env = map fst (toListFM env)

showEnv :: Show a => Env a -> String
showEnv env = unlines $ map show $ toListFM env 

-- pretty print the environment

pprintEnv :: PPrint Doc a => Env a -> Doc
pprintEnv env = pl global $+$ pl local_norm $+$ pl local_sys  where
    es = fmToList env
    (local,global) = partition (\(x,_) -> isDigit $ head (hsIdentString (hsNameIdent x)) ) es
    (local_sys,local_norm) = partition (\(x,_) -> last (hsIdentString (hsNameIdent x)) == '@' ) local
    pl es = vcat [((pprint a) <+> (text "::") <+> (pprint b)) | (a, b) <- es]

--   = vcat [((pprint a) <+> (text "::") <+> (pprint b)) | (a, b) <- toListFM env]

-- map a function over the elements of the environment
mapEnv :: (HsName -> e -> e') -> Env e -> Env e'
mapEnv f map = mapFM f map

--------------------------------------------------------------------------------

--instance (Ord a, Term a, Term b) => Term (FiniteMap a b) where
--    explode (x::(FiniteMap a b)) = TermRep (dx, tl, rb x) where
--        dx = toDyn x
--        tl = map explode $ fmToList x
--        rb (_::FiniteMap a b) l = toDyn $ listToFM ((map (\(TermRep (x,_,_)) -> fDyn x) l):: [(a,b)])



--_tc_FiniteMapTc = mkTyCon "FiniteMap"
--instance (Typeable a,Typeable b) => Typeable (FiniteMap a b) where
--    typeOf x = mkAppTy _tc_FiniteMapTc [ typeOf (geta x),typeOf (getb x) ]
--      where
--        geta :: FiniteMap a b -> a
--        geta = undefined
--        getb :: FiniteMap a b -> b
--        getb = undefined


