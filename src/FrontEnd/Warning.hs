module FrontEnd.Warning(
    Warning(..),
    MonadWarn(..),
    WarnType(..),
    warnIsFatal,
    processErrors,
    processErrors',
    warn,
    warnDoc,
    mkWarn,
    mkWarnD,
    addWarn,
    addWarnDoc,
    module FrontEnd.SrcLoc,
    -- IO monad
    getIOErrors,
    processIOErrors,
    printIOErrors
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.IORef
import System.IO
import System.IO.Unsafe
import Support.Exit
import Util.Std
import qualified Text.PrettyPrint.HughesPJ as P

import FrontEnd.SrcLoc(SrcSpan(..),WithSrcLoc(..),MonadSetSrcLoc(..),SrcLoc(..),MonadSrcLoc(..),bogusASrcLoc,SLM(..))
import Name.Name
import Options
import PackedString
import Util.Gen
import Util.DocLike
import qualified Util.Seq as Seq

data Warning = Warning {
    warnSrcLoc  :: !SrcLoc,
    warnType    :: WarnType,
    warnMessage :: P.Doc
    }

class (Applicative m,Monad m) => MonadWarn m where
    addWarning :: Warning -> m ()
    addWarning w = fail $ show w

addWarn :: (MonadWarn m, MonadSrcLoc m) => WarnType -> String -> m ()
addWarn t m = do
    sl <- getSrcLoc
    warn sl t m

addWarnDoc :: (MonadWarn m, MonadSrcLoc m) => WarnType -> P.Doc -> m ()
addWarnDoc t m = do
    sl <- getSrcLoc
    warnDoc sl t m

warn :: MonadWarn m => SrcLoc -> WarnType -> String -> m ()
warn warnSrcLoc warnType msg = addWarning Warning { warnMessage = text msg, .. }

warnDoc :: MonadWarn m => SrcLoc -> WarnType -> P.Doc -> m ()
warnDoc warnSrcLoc warnType msg = addWarning Warning { warnMessage = msg, .. }

mkWarn :: SrcLoc -> WarnType -> String -> Warning
mkWarn warnSrcLoc warnType msg = Warning { warnMessage = text msg, .. }
mkWarnD :: SrcLoc -> WarnType -> P.Doc -> Warning
mkWarnD warnSrcLoc warnType msg = Warning { warnMessage = msg, .. }

getIOErrors :: IO [Warning]
getIOErrors = do
    ws <- readIORef ioWarnings
    writeIORef ioWarnings []
    return ws

processIOErrors :: String -> IO ()
processIOErrors s = do
    ws <- readIORef ioWarnings
    unless (null ws || null s) $ do
        hFlush stdout
        hFlush stderr
        putErrLn $ "Diagnostics from " ++ s
    processErrors' True ws
    writeIORef ioWarnings []

-- | just show IO errors and return whether it would have died
printIOErrors :: IO Bool
printIOErrors = do
    ws <- readIORef ioWarnings
    b <- processErrors' False ws
    writeIORef ioWarnings []
    return b

processErrors :: [Warning] -> IO ()
processErrors ws = processErrors' True ws >> return ()

processErrors' :: Bool -> [Warning] -> IO Bool
processErrors' _ [] = return False
processErrors' doDie ws = do
    putErrLn "\n--"
--    mapM_ s (nub ws)
    mapM_ s ws
    when (die && doDie) (exitWith exitCode)
    return die
    where
    s Warning { warnSrcLoc = srcLoc@SrcLoc { .. }, .. }
        | srcLoc == bogusASrcLoc = putErrLn $ msg warnType warnMessage
        | srcLocLine == -1 = putErrLn $ P.render $ (text (unpackPS srcLocFileName) <> colon $$ (P.nest 4 warnMessage))
        | otherwise = putErrLn $ P.render $  (text (unpackPS srcLocFileName) <> colon <> (tshow srcLocLine) <>  colon  $$ P.nest 4 warnMessage)
    die =  not (optKeepGoing options) && exitCode /= ExitSuccess
    exitCode = maximum (ExitSuccess:concatMap (maybeToList . warnIsFatal . warnType) ws)
    msg t m = P.render m -- (if warnIsFatal t then "Error: " else "Warning: ") ++ m

data WarnType
    = AmbiguousExport Module [Name]
    | AmbiguousName Name [Name]

    | LexError
    | ParseError
    | ParseInfo

    | InvalidDecl
    | InvalidExp
    | CodeWarning

    | MissingModule Module (Maybe String)
    | MultiplyDefined Name [SrcLoc]

    | TypeSynonymPartialAp
    | TypeSynonymRecursive
    | UndefinedName Name
    | UnknownDeriving [Class]
    | UnknownOption
    | UnknownPragma PackedString
    | UnsupportedFeature
    -- failure via 'fail' monad method
    | WarnFailure
    -- type system errors
    | OccursCheck
    | UnificationError
    | UnexpectedType
    deriving(Eq,Ord)

warnIsFatal w = f w where
    f AmbiguousExport {} = Just exitCodeCodeError
    f AmbiguousName {} =  Just exitCodeCodeError
    f InvalidDecl {} =  Just exitCodeCodeError
    f InvalidExp {} =  Just exitCodeCodeError
    f MissingModule {} =  Just exitCodeCodeError
    f WarnFailure {} =  Just exitCodeFailure
    f MultiplyDefined {} = Just exitCodeCodeError
    f TypeSynonymPartialAp {} = Just exitCodeCodeError
    f TypeSynonymRecursive {} = Just exitCodeCodeError
    f UndefinedName {} = Just exitCodeCodeError
    f UnknownDeriving {} = Just exitCodeCodeError
    f UnsupportedFeature {} = Just exitCodeCodeError

    f LexError {} = Just exitCodeLexError
    f ParseError {} = Just exitCodeParseError

    f OccursCheck {} = Just exitCodeTypeError
    f UnificationError {} = Just exitCodeTypeError
    f UnexpectedType {} = Just exitCodeTypeError
    f _ = Nothing

instance Show Warning where
    show  Warning { warnSrcLoc = sl, warnType = t, warnMessage = m }
        | sl == bogusASrcLoc =  msg t m
    show  Warning { warnSrcLoc = SrcLoc { srcLocFileName = fn, srcLocLine = l },
                                          warnType = t ,warnMessage = m } =
         (unpackPS fn ++ ":" ++ (show l) ++  ":"  ++ msg t m)
msg t m = P.render m -- (if warnIsFatal t then "Error: " else "Warning: ") ++ m

_warnings = [
    ("deprecations"         ,"warn about uses of functions & types that are deprecated")        ,
    ("duplicate-exports"    ,"warn when an entity is exported multiple times")                  ,
    ("hi-shadowing"         ,"warn when a .hi file in the current directory shadows a library") ,
    ("incomplete-patterns"  ,"warn when a pattern match could fail")                            ,
    ("misc"                 ,"enable miscellaneous warnings")                                   ,
    ("missing-fields"       ,"warn when fields of a record are uninitialised")                  ,
    ("missing-methods"      ,"warn when class methods are undefined")                           ,
    ("missing-signatures"   ,"warn about top-level functions without signatures")               ,
    ("name-shadowing"       ,"warn when names are shadowed")                                    ,
    ("overlapping-patterns" ,"warn about overlapping patterns")                                 ,
    ("simple-patterns"      ,"warn about lambda-patterns that can fail")                        ,
    ("type-defaults"        ,"warn when defaulting happens")                                    ,
    ("unused-binds"         ,"warn about bindings that are unused")                             ,
    ("unused-imports"       ,"warn about unnecessary imports")                                  ,
    ("unused-matches"       ,"warn about variables in patterns that aren't used")
    ]

----------------
-- Warning monad
----------------

{-# NOINLINE ioWarnings #-}
ioWarnings :: IORef [Warning]
ioWarnings = unsafePerformIO $ newIORef []

instance MonadWarn m => MonadWarn (SLM m) where
    addWarning w = SLM $ addWarning w
instance MonadWarn IO where
    addWarning w = modifyIORef ioWarnings (w:)
instance MonadWarn (Writer [Warning]) where
    addWarning w = tell [w]
instance MonadWarn (Writer (Seq.Seq Warning)) where
    addWarning w = tell $ Seq.singleton w
instance MonadWarn Identity
instance MonadWarn m => MonadWarn (ReaderT a m) where
    addWarning w = lift $ addWarning w

instance (Applicative (t m),Monad m, Monad (t m), MonadTrans t, MonadWarn m) => MonadWarn (t m) where
    addWarning x = lift (addWarning x)
