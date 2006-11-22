module E.Program where

import Control.Monad.Identity
import Data.Monoid
import List
import qualified Data.Map as Map

import FrontEnd.Class
import DataConstructors
import E.E
import E.TypeCheck
import GenUtil
import Name.Id
import Name.Name
import Options
import qualified Stats


data Program = Program {
    progExternalNames :: IdSet,
    progClassHierarchy :: ClassHierarchy,
    progCombinators :: [(TVr,[TVr],E)],
    progDataTable :: DataTable,
    progEntryPoints :: [TVr],
    progMainEntry :: TVr,
    progModule :: Module,
    progClosed :: Bool,           -- ^ whether the universe is closed other than the entry points and external names
    progPasses :: [String],       -- ^ record of passes the program goes through
    progUsedIds :: IdSet,         -- ^ filled in by occurance info collection
    progFreeIds :: IdSet,         -- ^ filled in by occurance info collection
    progStats :: Stats.Stat
    }


program = Program {
    progExternalNames = mempty,
    progClassHierarchy = mempty,
    progCombinators = mempty,
    progDataTable = mempty,
    progEntryPoints = mempty,
    progMainEntry = tvr,
    progClosed = False,
    progModule = mainModule,
    progPasses = [],
    progUsedIds = mempty,
    progFreeIds = mempty,
    progStats = mempty
    }

intercalate x xs = concat (intersperse x xs)

programDs :: Program -> [(TVr,E)]
programDs prog = [ (t,foldr ELam e as)  | (t,as,e) <- progCombinators prog]

programSetDs :: [(TVr,E)] -> Program -> Program
programSetDs ds prog | flint && hasRepeatUnder (tvrIdent . fst) ds = error $ "programSetDs: program has redundant definitions: \n" ++ intercalate "\n"  (sort $ map (show . tvrShowName . fst) ds)
programSetDs ds prog | flint && any even (map (tvrIdent . fst) ds) = error $ "programSetDs: trying to set non unique top level name: \n" ++ intercalate "\n"  (sort $ map (show . tvrShowName . fst) ds)
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
programEsMap prog = runIdentity $ do
    let f d@(v,_) = case fromId (tvrIdent v) of
            Just n -> return (n,d)
            Nothing -> fail $ "Program.programEsMap: top level var with temporary name " ++ show v
    xs <- mapM f (programDs prog)
    return (Map.fromList xs)

-- | note, this will reset your entry points
programSetE :: E -> Program -> Program
programSetE ELetRec { eDefs = ds, eBody = EVar v } prog = programSetDs ds prog { progMainEntry = v }
programSetE ELetRec { eDefs = ds, eBody = mainBody } prog = programSetDs ((main,mainBody):ds) prog { progEntryPoints = [main], progMainEntry = main } where
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


