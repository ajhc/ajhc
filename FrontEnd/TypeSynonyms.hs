
module TypeSynonyms (
    removeSynonymsFromType,
    declsToTypeSynonyms,
    TypeSynonyms,
    showSynonym
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Monoid
import List
import qualified Data.Map as Map

import Binary
import Doc.DocLike
import FrontEnd.SrcLoc
import GenUtil
import HsErrors
import HsSyn
import Name.Name
import Util.HasSize
import Warning


newtype TypeSynonyms = TypeSynonyms (Map.Map Name ([HsName], HsType, SrcLoc))
    deriving(Monoid,Binary,HasSize)

showSynonym :: (DocLike d,Monad m) => (HsType -> d) -> Name -> TypeSynonyms -> m d
showSynonym pprint n (TypeSynonyms m) = do
    (ns, t, _) <- Map.lookup n m
    return $ hsep (tshow n:map tshow ns) <+> text "=" <+> pprint t

-- | convert a set of type synonym declarations to a synonym map used for efficient synonym
-- expansion

declsToTypeSynonyms :: [HsDecl] -> TypeSynonyms
declsToTypeSynonyms ts = TypeSynonyms $ Map.fromList [ (toName TypeConstructor name,( args , quantifyHsType args (HsUnQualType t) , sl)) | (HsTypeDecl sl name args t) <- ts]

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
    fv x = mapHsTypeHsType (\x -> fv x >> return x) x >> return ()


evalTypeSyms :: MonadWarn m => TypeSynonyms -> HsType -> m HsType
evalTypeSyms (TypeSynonyms tmap) t = eval [] t where
    eval stack x@(HsTyCon n) | Just (args, t, sl) <- Map.lookup (toName TypeConstructor n) tmap = do
        let excess = length stack - length args
        if (excess < 0) then do
            warn sl "type-synonym-partialap" ("Partially applied typesym:" <+> show n <+> "need" <+> show (- excess) <+> "more arguments.")
            unwind x stack
          else do
            eval (drop (length args) stack) (subst (Map.fromList [(a,s) | a <- args | s <- stack]) t)
    eval stack (HsTyApp t1 t2) = eval (t2:stack) t1
    eval stack x = do
        t <- mapHsTypeHsType (eval []) x
        unwind t stack
    unwind t [] = return t
    unwind t (t1:rest) = do
        t1' <- eval [] t1
        unwind (HsTyApp t t1') rest
    subst sm (HsTyForall vs t) = HsTyForall vs  t { hsQualTypeType =  subst (foldr ($) sm (map (\v m -> Map.delete (hsTyVarBindName v) m) vs)) (hsQualTypeType t) }
    subst sm (HsTyVar n) | Just v <- Map.lookup n sm = v
    subst sm t = runIdentity $ mapHsTypeHsType (return . subst sm) t


