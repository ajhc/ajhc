{-------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)

        Module:                 TypeUtils

        Description:            Utility functions for manipulating types,
                                and converting between the syntactic
                                representation of types and the internal
                                representation of types.

        Primary Authors:        Bernie Pope

        Notes:                  See the file License for license information

-------------------------------------------------------------------------------}

module TypeUtils (flattenLeftTypeApplication) where

import HsSyn
import Representation
import Type           (tv,tTTuple)
import FrontEnd.KindInfer
import Atom
import Name.Name
import Control.Monad.Identity

-- one sig can be given to multiple names, hence
-- the multiple assumptions in the output

{-
aHsTypeSigToAssumps :: KindEnv -> HsDecl -> [Assump]
aHsTypeSigToAssumps kt sig@(HsTypeSig _ names qualType)
   = [n :>: scheme | n <- names]
   where
   scheme = aHsQualTypeToScheme newEnv qualType
   --newEnv = kiHsQualType kt qualType
   newEnv = kt
-}


--aHsAsstToPred :: KindEnv -> HsAsst -> Pred
--aHsAsstToPred kt (className, varName)
   -- = IsIn className (TVar $ Tyvar varName (kindOf varName kt))
--   = IsIn className (TVar $ tyvar varName (kindOf className kt) Nothing)

-- one sig can be given to multiple names, hence
-- the multiple assumptions in the output

{-
aHsTypeSigToAssumps :: KindEnv -> HsDecl -> [Assump]
aHsTypeSigToAssumps kt sig@(HsTypeSig _ names qualType) = [ toName Val n :>: scheme | n <- names] where
    Identity scheme = hsQualTypeToScheme kt qualType
   --scheme = aHsQualTypeToScheme newEnv qualType
   --newEnv = kiHsQualType kt qualType

   converts leftmost type applications into lists

   (((TC v1) v2) v3) => [TC, v1, v2, v3]

-}
flattenLeftTypeApplication :: HsType -> [HsType]
flattenLeftTypeApplication t
   = flatTypeAcc t []
   where
   flatTypeAcc (HsTyApp t1 t2) acc
      = flatTypeAcc t1 (t2:acc)
   flatTypeAcc nonTypApp acc
      = nonTypApp:acc

-- qualifies a type assumption to a given module, unless
-- it is already qualified

--qualifyAssump :: Module -> Assump -> Assump
--qualifyAssump mod assump
--   | isQual ident = assump  -- do nothing
--   | otherwise = makeAssump newQualIdent scheme
--   where
--   scheme :: Scheme
--   scheme = assumpScheme assump
--   ident :: Name
--   ident = assumpId assump
--   newQualIdent :: Name
--   newQualIdent = Qual mod $ HsIdent $ show ident

