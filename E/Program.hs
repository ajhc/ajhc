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
    progModule :: Module,
    progStats :: Stats.Stat
    }


program = Program {
    progClassHierarchy = mempty,
    progCombinators = mempty,
    progDataTable = mempty,
    progEntryPoints = mempty,
    progMainEntry = tvr,
    progModule = mainModule,
    progStats = mempty
    }


programDs :: Program -> [(TVr,E)]
programDs prog = [ (t,foldr ELam e as)  | (t,as,e) <- progCombinators prog]

programSetDs :: [(TVr,E)] -> Program -> Program
programSetDs ds prog = prog {
    progMainEntry = f (progMainEntry prog),
    progEntryPoints = map f (progEntryPoints prog),
    progCombinators = [ (t,as,body) | (t,e) <- ds, let (body,as) = fromLam e ]
    } where
    f tvr | Just n <- Map.lookup (tvrIdent tvr) mp = n
          | otherwise = tvr
    mp = Map.fromList [ (tvrIdent t,t) | (t,_) <- ds ]

programAddDs :: [(TVr,E)] -> Program -> Program
programAddDs ds prog = prog { progCombinators = [ (t,as,body) | (t,e) <- ds, let (body,as) = fromLam e ] ++ progCombinators prog }

programE :: Program -> E
programE prog = ELetRec (programDs prog) (EVar (progMainEntry prog))

programEsMap :: Program -> Map.Map Name (TVr,E)
programEsMap prog = Map.fromList [ (runIdentity $ fromId (tvrIdent v),d) | d@(v,_) <- programDs prog ]

-- | note, this will reset your entry points
programSetE :: E -> Program -> Program
programSetE (ELetRec ds (EVar v)) prog = programSetDs ds prog { progMainEntry = v }
programSetE (ELetRec ds mainBody) prog = programSetDs ((main,mainBody):ds) prog { progEntryPoints = [main], progMainEntry = main } where
    main = (tVr num (typeInfer (progDataTable prog) mainBody))
    Just num = List.find (`notElem` [ n  | (TVr { tvrIdent = n },_) <- ds ]) [toId $ toName Val (show $ progModule prog,"main" ++ show n) |  n <- [1 :: Int ..] ]
programSetE e prog = prog { progCombinators = [(main,as,mainBody)], progEntryPoints = [main], progMainEntry = main } where
    (mainBody,as) = fromLam e
    main = tVr (toId $ toName Val (show $ progModule prog,"main")) (typeInfer (progDataTable prog) e)

programMapBodies f prog = do
     ds <- sequence [ f e >>= return . (,) t | (t,e) <- programDs prog ]
     return $ programSetDs ds prog

programMapDs f prog = do
     ds <- mapM f (programDs prog)
     return $ programSetDs ds prog

programMapDs_ f prog = mapM_ f (programDs prog)

{-
programMapRecGroups :: Monad m => ([(TVr,E)] -> m [(TVr,E)]) -> Program -> m Program
programMapRecGroups f prog = do
    let pds = programDs prog
-}

