{-------------------------------------------------------------------------------

    this is not what this module does at all.

        Copyright:              The Hatchet Team (see file Contributors)

        Module:                 MultiModuleBasics

        Description:            More Support code for type checking multi-module
                                programs.

        Primary Authors:        Bryn Humberstone

        Notes:                  See the file License for license information

-------------------------------------------------------------------------------}

-- TODO - get rid of

module MultiModuleBasics where

import HsSyn
import qualified Data.Map as M
import Options
import Name
import FrontEnd.Infix

--------------------------------------------------------------------------------


data ModInfo = ModInfo {
    modInfoName :: Module,
    modInfoDefs :: [(Name,SrcLoc,[Name])],
    modInfoConsArity :: [(Name,Int)],
    modInfoExport :: [Name],
    modInfoImport :: [(Name,[Name])],
    modInfoHsModule :: HsModule,
    modInfoOptions :: Opt
    }
   {-! derive: update !-}

instance Eq ModInfo where
    a == b = modInfoName a == modInfoName b

instance Ord ModInfo where
    compare a b = compare (modInfoName a) (modInfoName b)


{-

data ModEnv = ModEnv {
    modEnvModules :: M.Map Module ModInfo,
    modEnvVarAssumptions :: Env Scheme,          -- used for typechecking
    modEnvDConsAssumptions :: Env Scheme,        -- used for typechecking
    modEnvAllAssumptions :: M.Map Name Scheme,          -- used for code generation
    modEnvFixities :: FixityMap,
    modEnvKinds :: KindEnv,                      -- used for typechecking
    modEnvClassHierarchy :: ClassHierarchy,
    modEnvTypeSynonyms :: TypeSynonyms,
    modEnvLiftedInstances :: M.Map HsName HsDecl
    }
   {-! derive: update, Monoid !-}

emptyModEnv :: ModEnv
emptyModEnv = mempty

-}
