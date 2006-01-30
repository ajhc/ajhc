module E.Program where

import Control.Monad.Identity
import Data.Monoid
import List
import qualified Data.Map as Map

import Class
import DataConstructors
import E.E
import E.FreeVars
import E.TypeCheck
import Name.Name
import qualified Stats
import Util.Graph


data Program = Program {
    progClassHierarchy :: ClassHierarchy,
    progCombinators :: [(TVr,[TVr],E)],
    progDataTable :: DataTable,
    progEntryPoints :: [TVr],
    progMainEntry :: TVr,
    progStats :: Stats.Stat
    }


program = Program {
    progClassHierarchy = mempty,
    progCombinators = mempty,
    progDataTable = mempty,
    progEntryPoints = mempty,
    progMainEntry = tvr,
    progStats = mempty
    }


programDs :: Program -> [(TVr,E)]
programDs prog = [ (t,foldr ELam e as)  | (t,as,e) <- progCombinators prog]

programSetDs :: [(TVr,E)] -> Program -> Program
programSetDs ds prog = prog { progCombinators = [ (t,as,body) | (t,e) <- ds, let (body,as) = fromLam e ] }

programAddDs :: [(TVr,E)] -> Program -> Program
programAddDs ds prog = prog { progCombinators = [ (t,as,body) | (t,e) <- ds, let (body,as) = fromLam e ] ++ progCombinators prog }

programE :: Program -> E
programE prog = ELetRec (programDs prog) (EVar (progMainEntry prog))

programEsMap :: Program -> Map.Map Name (TVr,E)
programEsMap prog = Map.fromList [ (runIdentity $ fromId (tvrIdent v),d) | d@(v,_) <- programDs prog ]

programSetE :: E -> Program -> Program
programSetE (ELetRec ds (EVar v)) prog = programSetDs ds prog { progMainEntry = v }
programSetE (ELetRec ds mainBody) prog = programSetDs ((main,mainBody):ds) prog { progMainEntry = main } where
    main = (tVr num (typeInfer (progDataTable prog) mainBody))
    Just num = List.find (`notElem` [ n  | (TVr { tvrIdent = n },_) <- ds ]) [2,4 ..]
programSetE e prog = prog { progCombinators = [(main,as,mainBody)], progMainEntry = main } where
    (mainBody,as) = fromLam e
    main = tVr 2 (typeInfer (progDataTable prog) e)

programMapBodies f prog = do
     ds <- sequence [ f e >>= return . (,) t | (t,e) <- programDs prog ]
     return $ programSetDs ds prog


