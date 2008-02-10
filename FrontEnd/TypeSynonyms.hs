
module TypeSynonyms (
    removeSynonymsFromType,
    declsToTypeSynonyms,
    TypeSynonyms,
    restrictTypeSynonyms,
    showSynonym
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Monoid
import Data.Binary
import List
import qualified Data.Map as Map

import Doc.DocLike
import FrontEnd.SrcLoc
import GenUtil
import Util.UniqueMonad
import FrontEnd.Syn.Traverse
import HsSyn
import Name.Name
import Util.HasSize
import FrontEnd.Warning
import Support.MapBinaryInstance


newtype TypeSynonyms = TypeSynonyms (Map.Map Name ([HsName], HsType, SrcLoc))
    deriving(Monoid,HasSize)

instance Binary TypeSynonyms where
    put (TypeSynonyms ts) = putMap ts
    get = fmap TypeSynonyms getMap

restrictTypeSynonyms :: (Name -> Bool) -> TypeSynonyms -> TypeSynonyms
restrictTypeSynonyms f (TypeSynonyms fm) = TypeSynonyms (Map.filterWithKey (\k _ -> f k) fm)

showSynonym :: (DocLike d,Monad m) => (HsType -> d) -> Name -> TypeSynonyms -> m d
showSynonym pprint n (TypeSynonyms m) = do
    (ns, t, _) <- Map.lookup n m
    return $ hsep (tshow n:map tshow ns) <+> text "=" <+> pprint t

-- | convert a set of type synonym declarations to a synonym map used for efficient synonym
-- expansion

declsToTypeSynonyms :: [HsDecl] -> TypeSynonyms
declsToTypeSynonyms ts = TypeSynonyms $ Map.fromList $
    [ (toName TypeConstructor name,( args , quantifyHsType args (HsQualType [] t) , sl)) | (HsTypeDecl sl name args' t) <- ts, let args = [ n | ~(HsTyVar n) <- args'] ]
     ++ [ (toName TypeConstructor name,( args , HsTyAssoc, sl)) | (HsClassDecl _ _ ds) <- ts,(HsTypeDecl sl name args' _) <- ds, let args = [ n | ~(HsTyVar n) <- args'] ]

removeSynonymsFromType :: MonadWarn m => TypeSynonyms -> HsType -> m HsType
removeSynonymsFromType syns t = evalTypeSyms  syns t

quantifyHsType :: [HsName] -> HsQualType -> HsType
quantifyHsType inscope t
  | null vs, null (hsQualTypeHsContext t) = hsQualTypeType t
  | otherwise  = HsTyForall vs t   where
    vs = map g $ snub (execWriter (fv (hsQualTypeType t))) \\ inscope
    g n = hsTyVarBind { hsTyVarBindName = n }
    fv (HsTyVar v) = tell [v]
    fv (HsTyForall vs qt) = tell $ snub (execWriter (fv $ hsQualTypeType qt)) \\ map hsTyVarBindName vs
    fv (HsTyExists vs qt) = tell $ snub (execWriter (fv $ hsQualTypeType qt)) \\ map hsTyVarBindName vs
    fv x = traverseHsType (\x -> fv x >> return x) x >> return ()


evalTypeSyms :: MonadWarn m => TypeSynonyms -> HsType -> m HsType
evalTypeSyms (TypeSynonyms tmap) t = execUniqT 1 (eval [] t) where
    eval stack x@(HsTyCon n) | Just (args, t, sl) <- Map.lookup (toName TypeConstructor n) tmap = do
        let excess = length stack - length args
        if (excess < 0) then do
            lift $ warn sl "type-synonym-partialap" ("Partially applied typesym:" <+> show n <+> "need" <+> show (- excess) <+> "more arguments.")
            unwind x stack
          else case t of
            HsTyAssoc -> unwind x stack
            _ -> do
                st <- subst (Map.fromList [(a,s) | a <- args | s <- stack]) t
                eval (drop (length args) stack) st
    eval stack (HsTyApp t1 t2) = eval (t2:stack) t1
    eval stack x = do
        t <- traverseHsType (eval []) x
        unwind t stack
    unwind t [] = return t
    unwind t (t1:rest) = do
        t1' <- eval [] t1
        unwind (HsTyApp t t1') rest
    subst sm (HsTyForall vs t) = do
        ns <- mapM (const newUniq) vs
        let nvs = [ (hsTyVarBindName v,v { hsTyVarBindName = hsNameIdent_u (hsIdentString_u ((show n ++ "00") ++)) (hsTyVarBindName v)})| (n,v) <- zip ns vs ]
            nsm = Map.fromList [ (v,HsTyVar $ hsTyVarBindName t)| (v,t) <- nvs] `Map.union` sm
        t' <- substqt nsm t
        return $ HsTyForall (snds nvs)  t'
    subst sm (HsTyExists vs t) = do
        ns <- mapM (const newUniq) vs
        let nvs = [ (hsTyVarBindName v,v { hsTyVarBindName = hsNameIdent_u (hsIdentString_u ((show n ++ "00") ++)) (hsTyVarBindName v)})| (n,v) <- zip ns vs ]
            nsm = Map.fromList [ (v,HsTyVar $ hsTyVarBindName t)| (v,t) <- nvs] `Map.union` sm
        t' <- substqt nsm t
        return $ HsTyExists (snds nvs)  t'
    subst (sm::(Map.Map HsName HsType))  (HsTyVar n) | Just v <- Map.lookup n sm = return v
    subst sm t = traverseHsType (subst sm) t
    substqt sm qt@HsQualType { hsQualTypeContext = ps, hsQualTypeType = t } = do
        t' <- subst sm t
        let f (HsAsst c xs) = return (HsAsst c (map g xs))
            f (HsAsstEq a b) = do
                a' <- subst sm a
                b' <- subst sm b
                return (HsAsstEq a' b')
            g n =  case Map.lookup n sm of Just (HsTyVar n') -> n' ; _ -> n
        ps' <- mapM f ps -- = [ case Map.lookup n sm of Just (HsTyVar n') -> (c,n') ; _ -> (c,n) | (c,n) <- ps ]

        return qt { hsQualTypeType = t', hsQualTypeContext = ps' }


