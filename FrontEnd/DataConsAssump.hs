{-------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)

        Module:                 DataConsAssump

        Description:            Computes the type assumptions of data
                                constructors in a module

                                For example:
                                        MyCons :: a -> MyList a
                                        Just :: a -> Maybe a
                                        True :: Bool

                                Note Well:

                                from section 4.2 of the Haskell Report:

                                "These declarations may only appear at the
                                 top level of a module."

        Primary Authors:        Bernie Pope

        Notes:                  See the file License for license information

-------------------------------------------------------------------------------}

module DataConsAssump (dataConsEnv) where

import Control.Monad.Identity
import qualified Data.Map as Map

import HsSyn
import Representation
import FrontEnd.Tc.Type
import Name.Name
import Type                     (Types (..))
import FrontEnd.KindInfer

--------------------------------------------------------------------------------

dataConsEnv :: Module -> KindEnv -> [HsDecl] -> Map.Map Name Sigma
dataConsEnv modName kt decls
   = Map.unions $ map (dataDeclEnv modName kt) decls


-- we should only apply this function to data decls and newtype decls
-- howver the fall through case is just there for completeness

dataDeclEnv :: Module -> KindEnv -> (HsDecl) -> Map.Map Name Sigma
dataDeclEnv modName kt (HsDataDecl _sloc context typeName args condecls _)
   = Map.unions $ map (conDeclType modName kt preds resultType) $ condecls
   where
   typeName' = toName TypeConstructor typeName
   typeKind = kindOf typeName' kt
   resultType = foldl TAp tycon argVars
   tycon = TCon (Tycon typeName' typeKind)
   argVars = map fromHsNameToTyVar $ zip argKinds args
   argKinds = init $ unfoldKind typeKind
   fromHsNameToTyVar :: (Kind, HsName) -> Type
   fromHsNameToTyVar (k, n)
      = TVar (tyvar (toName TypeVal n) k)
   preds = hsContextToPreds kt context

dataDeclEnv modName kt (HsNewTypeDecl _sloc context typeName args condecl _)
   = conDeclType modName kt preds resultType condecl
   where
   typeName' = toName TypeConstructor typeName
   typeKind = kindOf typeName' kt
   resultType = foldl TAp tycon argVars
   tycon = TCon (Tycon typeName' typeKind)
   argVars = map fromHsNameToTyVar $ zip argKinds args
   argKinds = init $ unfoldKind typeKind
   fromHsNameToTyVar :: (Kind, HsName) -> Type
   fromHsNameToTyVar (k, n)
      = TVar (tyvar (toName TypeVal n) k)
   preds = hsContextToPreds kt context

dataDeclEnv _modName _kt _anyOtherDecl
   = Map.empty


hsContextToPreds :: KindEnv -> HsContext -> [Pred]
hsContextToPreds kt assts = map (hsAsstToPred kt) assts

-- XXX we ignore predicates on data constructors because they don't mean anything

conDeclType :: Module -> KindEnv -> [Pred] -> Type -> HsConDecl -> Map.Map Name Sigma
conDeclType modName kt preds tResult (HsConDecl { hsConDeclName = conName, hsConDeclConArg = bangTypes })
   = Map.singleton (toName DataConstructor conName) $ tForAll (tv qualConType) qualConType
   where
   conType = foldr fn tResult (map (bangTypeToType kt) bangTypes)
   qualConType = preds :=> conType
conDeclType modName kt preds tResult rd@HsRecDecl { hsConDeclName = conName }
   = Map.singleton (toName DataConstructor conName) $ tForAll (tv qualConType) qualConType
   where
   conType = foldr fn tResult (map (bangTypeToType kt) (hsConDeclArgs rd))
   qualConType = preds :=> conType

bangTypeToType :: KindEnv -> HsBangType -> Type
bangTypeToType kt (HsBangedTy t) = runIdentity $ hsTypeToType kt t
bangTypeToType kt (HsUnBangedTy t) = runIdentity $ hsTypeToType kt t

