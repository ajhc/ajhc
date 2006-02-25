module FrontEnd.TiData where

import HsSyn
import Data.Map as Map
import Representation
import Name.Name
import Options

-- Extra data produced by the front end, used to fill in the Ho file.
data TiData = TiData {
    tiDataLiftedInstances :: Map.Map Name HsDecl,
    tiDataModules :: [(Module,HsModule)],
    tiModuleOptions :: [(Module,Opt)],
    tiAllAssumptions :: Map.Map Name Scheme
}
