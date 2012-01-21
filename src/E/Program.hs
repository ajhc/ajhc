module E.Program where

import Control.Monad.Identity
import Data.Monoid
import List
import Maybe
import qualified Data.Map as Map

import DataConstructors
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.Show
import E.TypeCheck
import Name.Id
import Name.Name
import Options
import System.IO
import Util.Gen hiding(putErrLn)
import Util.SetLike
import qualified FlagDump as FD
import qualified Stats

data ProgramType = SubProgram Bool | MainProgram | MainComponent

data Program = Program {
    progExternalNames  :: IdSet,
    progCombinators    :: [Comb],
    progDataTable      :: DataTable,
    progEntry          :: IdSet,
    progMain           :: Id,
    progModule         :: Module,
    progPasses         :: [String],    -- ^ record of passes the program goes through
    progUsedIds        :: IdSet,       -- ^ filled in by occurance info collection
    progFreeIds        :: IdSet,       -- ^ filled in by occurance info collection
    progSeasoning      :: IdSet,       -- ^ these ids are depended on by external names via RULES
    progType           :: ProgramType,
    progCombMap        :: IdMap Comb,  -- progCombMap is always (fromList . progCombinators)
    progStats          :: !Stats.Stat
    }

program = Program {
    progExternalNames  = mempty,
    progCombinators    = mempty,
    progDataTable      = mempty,
    progEntry          = mempty,
    progMain           = emptyId,
    progModule         = mainModule,
    progPasses         = [],
    progUsedIds        = mempty,
    progFreeIds        = mempty,
    progSeasoning      = mempty,
    progType           = MainProgram,
    progCombMap        = mempty,
    progStats          = mempty
    }

progEntryPoints prog = map combHead $ concatMap (progComb prog) (toList $ progEntry prog)
progMainEntry prog = combHead . runIdentity $ progComb prog (progMain prog)

progComb :: Monad m => Program -> Id -> m Comb
progComb prog x = case x `mlookup`  progCombMap prog of
    Nothing -> fail $ "progComb: can't find '" ++ show (tvrShowName tvr { tvrIdent = x }) ++  "'"
    Just c -> return c

programDs :: Program -> [(TVr,E)]
programDs prog = [ (t,e)  | Comb { combHead = t, combBody = e }  <- progCombinators prog]

progCombinators_u f prog = programUpdate prog { progCombinators = f $ progCombinators prog }
progCombinators_s cs prog = programUpdate prog { progCombinators = cs }

programUpdate ::  Program -> Program
programUpdate prog = check $ ucache prog where
    ds = progCombinators prog
    ucache prog = prog { progCombMap = fromList [ (combIdent c,c) | c <- ds ] }
    check x
        | not flint = x
        | hasRepeatUnder combIdent ds = error $ "programSetDs: program has redundant definitions: \n" ++ names
        | any (not . isJust . fromId) (map combIdent ds) = error $ "programSetDs: trying to set non unique top level name: \n" ++ names
        | otherwise = x
    names = intercalate "\n"  (sort $ map (show . tvrShowName . combHead) ds)

programSetDs' :: [(TVr,E)] -> Program -> Program
programSetDs' ds prog = progCombinators_s [ combRules_s (lupRules (tvrIdent t)) $ bindComb (t,e) | (t,e) <- ds ] prog where
    lupRules t = case mlookup t (progCombMap prog) of
        Just c -> combRules c
        Nothing -> mempty

programSetDs :: [(TVr,E)] -> Program -> Program
programSetDs ds prog = progCombinators_s [ bindComb (t,e) | (t,e) <- ds ] prog

programE :: Program -> E
programE prog = ELetRec (programDs prog) (EVar (progMainEntry prog))

programEsMap :: Monad m => Program -> m (Map.Map Name (TVr,E))
programEsMap prog = do
    let f d@(v,_) = case fromId (tvrIdent v) of
            Just n -> return (n,d)
            Nothing -> fail $ "Program.programEsMap: top level var with temporary name " ++ show v
    xs <- mapM f (programDs prog)
    return (Map.fromList xs)

programMapBodies :: Monad m => (E -> m E) -> Program -> m Program
programMapBodies f prog = do
    let f' (t,e) = f e >>= \e' -> return (t,e')
    programMapDs f' prog

programMapDs :: Monad m => ((TVr, E) -> m (TVr, E)) -> Program -> m Program
programMapDs f prog = do
    cs <- forM (progCombinators prog) $ \comb -> do
        (t,e) <- f (combHead comb,combBody comb)
        return . combHead_s t . combBody_s e $ comb
    return $ progCombinators_s cs prog

programMapDs_ :: Monad m => ((TVr,E) -> m ()) -> Program -> m ()
programMapDs_ f prog = mapM_ f (programDs prog)

hPrintProgram fh prog@Program {progCombinators = cs, progDataTable = dataTable } = do
    sequence_ $ intersperse (hPutStrLn fh "") [ hPrintCheckName fh dataTable v e | Comb { combHead = v, combBody = e } <- cs]
    when (progMain prog /= emptyId) $
        hPutStrLn fh $ "MainEntry: " ++ pprint (progMainEntry prog)
    when (progEntry prog /= singleton (progMain prog)) $
        hPutStrLn fh $ "EntryPoints: " ++ hsep (map pprint (progEntryPoints prog))

printProgram prog = hPrintProgram stderr prog

printCheckName'' = hPrintCheckName stderr

hPrintCheckName :: Handle -> DataTable -> TVr -> E -> IO ()
hPrintCheckName fh dataTable tvr e = do
    let (ty,pty) = case inferType dataTable [] e of
            Left err -> (Unknown,vcat $ map text (intersperse "---" $ tail err))
            Right ty -> (ty,pprint ty)
        tmatch = isJust $ match (const Nothing) [] ty (tvrType tvr)
    when (dump FD.EInfo || verbose2) $ hPutStrLn fh (show $ tvrInfo tvr)
    hPutStrLn fh (render $ hang 4 (pprint tvr <+> text "::" <+> (pprint $ tvrType tvr)))
    when (ty /= Unknown && (not tmatch || dump FD.EVerbose)) $
        hPutStrLn fh (render $ hang 4 (pprint tvr <+> text "::" <+> pty))
    hPutStrLn fh (render $ hang 4 (pprint tvr <+> equals <+> pprint e))
    when (ty == Unknown) $
        hPutStrLn fh (render $ hang 4 (pprint tvr <+> text "TypeError:" </> pty))
