module E.Program where

import Prelude hiding(putStrLn, putStr,print)
import Control.Monad.Identity
import Data.Monoid
import List
import Maybe
import qualified Data.Map as Map

import DataConstructors
import Doc.DocLike
import Doc.PPrint
import CharIO
import Doc.Pretty
import E.E
import E.Show
import E.TypeCheck
import FrontEnd.Class
import Util.Gen hiding(putErrLn)
import Name.Id
import Name.Name
import Options
import qualified FlagDump as FD
import qualified Stats


data ProgramType = SubProgram Bool | MainProgram | MainComponent

data Program = Program {
    progExternalNames :: IdSet,
    progClassHierarchy :: ClassHierarchy,
    progCombinators :: [(TVr,[TVr],E)],
    progDataTable :: DataTable,
    progEntryPoints :: [TVr],
    progMainEntry :: TVr,
    progModule :: Module,
    progPasses :: [String],       -- ^ record of passes the program goes through
    progUsedIds :: IdSet,         -- ^ filled in by occurance info collection
    progFreeIds :: IdSet,         -- ^ filled in by occurance info collection
    progType    :: ProgramType,
    progStats :: Stats.Stat
    }


program = Program {
    progExternalNames = mempty,
    progClassHierarchy = mempty,
    progCombinators = mempty,
    progDataTable = mempty,
    progEntryPoints = mempty,
    progMainEntry = tvr,
    progModule = mainModule,
    progPasses = [],
    progUsedIds = mempty,
    progFreeIds = mempty,
    progType = MainProgram,
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


printProgram prog@Program {progCombinators = cs, progDataTable = dataTable } = do
    sequence_ $ intersperse (putErrLn "") [ printCheckName'' dataTable v (foldr ELam e as) | (v,as,e) <- cs]
    when (progMainEntry prog /= tvr) $
        putErrLn $ "MainEntry: " ++ pprint (progMainEntry prog)
    when (progEntryPoints prog /= [progMainEntry prog]) $
        putErrLn $ "EntryPoints: " ++ hsep (map pprint (progEntryPoints prog))

printCheckName'' :: DataTable -> TVr -> E -> IO ()
printCheckName'' dataTable tvr e = do
    let (ty,pty) = case inferType dataTable [] e of
            Left err -> (Unknown,vcat $ map text (intersperse "---" $ tail err))
            Right ty -> (ty,pprint ty)
        tmatch = isJust $ match (const Nothing) [] ty (tvrType tvr)
    when (dump FD.EInfo || verbose2) $ putErrLn (show $ tvrInfo tvr)
    putErrLn (render $ hang 4 (pprint tvr <+> text "::" <+> (pprint $ tvrType tvr)))
    when (not tmatch || dump FD.EVerbose) $
        putErrLn (render $ hang 4 (pprint tvr <+> text "::" <+> pty))
    putErrLn (render $ hang 4 (pprint tvr <+> equals <+> pprint e))
