module FrontEnd.Warning(
    Warning(..),
    MonadWarn(..),
    WarnType(..),
    processErrors,
    warn,
    err,
    addWarn,
    -- IO monad
    processIOErrors,
    printIOErrors
    ) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.IORef
import System.IO.Unsafe

import FrontEnd.SrcLoc
import Name.Name
import Options
import PackedString
import StringTable.Atom
import Util.Gen

data Warning = Warning {
    warnSrcLoc  :: !SrcLoc,
    warnType    :: WarnType,
    warnMessage :: String
    } deriving(Eq,Ord)

class Monad m => MonadWarn m where
    addWarning :: Warning -> m ()
    addWarning w = fail $ show w

addWarn :: (MonadWarn m, MonadSrcLoc m) => WarnType -> String -> m ()
addWarn t m = do
    sl <- getSrcLoc
    warn sl t m

warn :: MonadWarn m => SrcLoc -> WarnType -> String -> m ()
warn s t m = addWarning Warning
    { warnSrcLoc = s, warnType = t, warnMessage = m }

err :: MonadWarn m => WarnType -> String -> m ()
err t m = warn bogusASrcLoc t m

pad n s = case length s of
    x | x >= n -> s
    x -> s ++ replicate (n - x) ' '

processIOErrors :: IO ()
processIOErrors = do
    ws <- readIORef ioWarnings
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
processErrors' doDie ws = putErrLn "" >> mapM_ s (snub ws) >> when (die && doDie) exitFailure >> return die where
--    ws' = filter ((`notElem` ignore) . warnType ) $ snub ws
    s Warning { warnSrcLoc = sl, warnType = t, warnMessage = m }
        | sl == bogusASrcLoc = putErrLn $ msg t m
    s Warning { warnSrcLoc = SrcLoc { srcLocFileName = fn, srcLocLine = -1 },
                warnType = t ,warnMessage = m } = putErrLn (unpackPS fn ++ ": "  ++ msg t m)
    s Warning { warnSrcLoc = SrcLoc { srcLocFileName = fn, srcLocLine = l },
                warnType = t ,warnMessage = m } = putErrLn (unpackPS fn ++ ":" ++ pad 3 (show l) ++  " - "  ++ msg t m)
    die = (any warnIsFatal (map warnType ws)) && not (optKeepGoing options)

data WarnType
    = AmbiguousExport Module [Name]
    | AmbiguousName Name [Name]
    | DuplicateInstances
    | InvalidDecl
    | MissingDep String
    | MissingModule Module
    | MultiplyDefined Name [SrcLoc]
    | InvalidFFIType
    | OccursCheck
    | PrimitiveBadType
    | PrimitiveUnknown Atom
    | TypeSynonymPartialAp
    | TypeSynonymRecursive
    | UndefinedName Name
    | UnificationError
    | UnknownDeriving [Class]
    | UnknownOption
    | UnknownPragma PackedString
    | UnsupportedFeature
    deriving(Eq,Ord)

warnIsFatal w = f w where
    f AmbiguousExport {} = True
    f AmbiguousName {} = True
    f InvalidDecl {} = True
    f InvalidFFIType {} = True
    f DuplicateInstances {} = True
    f MissingDep {} = True
    f MissingModule {} = True
    f MultiplyDefined {} = True
    f OccursCheck {} = True
    f TypeSynonymPartialAp {} = True
    f TypeSynonymRecursive {} = True
    f UndefinedName {} = True
    f UnificationError {} = True
    f UnknownDeriving {} = True
    f UnsupportedFeature {} = True
    f _ = False

instance Show Warning where
    show  Warning { warnSrcLoc = sl, warnType = t, warnMessage = m }
        | sl == bogusASrcLoc =  msg t m
    show  Warning { warnSrcLoc = SrcLoc { srcLocFileName = fn, srcLocLine = l },
                                          warnType = t ,warnMessage = m } =
         (unpackPS fn ++ ":" ++ pad 3 (show l) ++  " - "  ++ msg t m)
msg t m = (if warnIsFatal t then "Error: " else "Warning: ") ++ m

_warnings = [
    ("deprecations", "warn about uses of functions & types that are deprecated"),
    ("duplicate-exports", "warn when an entity is exported multiple times"),
    ("hi-shadowing", "warn when a .hi file in the current directory shadows a library"),
    ("incomplete-patterns", "warn when a pattern match could fail"),
    ("misc", "enable miscellaneous warnings"),
    ("missing-fields", "warn when fields of a record are uninitialised"),
    ("missing-methods", "warn when class methods are undefined"),
    ("missing-signatures", "warn about top-level functions without signatures"),
    ("name-shadowing", "warn when names are shadowed"),
    ("overlapping-patterns", "warn about overlapping patterns"),
    ("simple-patterns", "warn about lambda-patterns that can fail"),
    ("type-defaults", "warn when defaulting happens"),
    ("unused-binds", "warn about bindings that are unused"),
    ("unused-imports", "warn about unnecessary imports"),
    ("unused-matches", "warn about variables in patterns that aren't used")
    ]

----------------
-- Warning monad
----------------

{-# NOINLINE ioWarnings #-}
ioWarnings :: IORef [Warning]
ioWarnings = unsafePerformIO $ newIORef []

instance MonadWarn IO where
    addWarning w = modifyIORef ioWarnings (w:)
instance MonadWarn (Writer [Warning]) where
    addWarning w = tell [w]
instance MonadWarn Identity
instance MonadWarn m => MonadWarn (ReaderT a m) where
    addWarning w = lift $ addWarning w
