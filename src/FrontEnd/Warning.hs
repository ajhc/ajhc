module FrontEnd.Warning(
    Warning(..),
    MonadWarn(..),
    processErrors,
    warn,
    warnF,
    err,
    addDiag,
    addWarn,
    processIOErrors,
    printIOErrors
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.IORef
import FrontEnd.SrcLoc
import GenUtil
import List
import Options
import System.IO.Unsafe

{-# NOINLINE ioWarnings #-}
ioWarnings :: IORef [Warning]
ioWarnings = unsafePerformIO $ newIORef []

data Warning = Warning { warnSrcLoc :: !SrcLoc, warnType :: String, warnMessage :: String }
    deriving(Eq,Ord)

class Monad m => MonadWarn m where
    addWarning :: Warning -> m ()
    addWarning w = fail $ show w

-- If in the IO monad, just show the warning
instance MonadWarn IO where
    addWarning w = modifyIORef ioWarnings (w:)

instance MonadWarn (Writer [Warning]) where
    addWarning w = tell [w]
instance MonadWarn Identity where
    addWarning w = fail $ show w

addWarn t m = do
    sl <- getSrcLoc
    warn sl t m

addDiag s = warn bogusASrcLoc "diagnostic" s
warn s t m = addWarning (Warning { warnSrcLoc = s, warnType = t, warnMessage = m })
err t m = warn bogusASrcLoc t m
warnF fn t m  = warn bogusASrcLoc { srcLocFileName = fn } t m

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
processErrors' doDie ws = mapM_ s ws' >> when (die && doDie) exitFailure >> return die where
    ws' = filter ((`notElem` ignore) . warnType ) $ snub ws
    s Warning { warnSrcLoc = sl, warnType = t, warnMessage = m } | sl == bogusASrcLoc = putErrLn $ msg t m
    s Warning { warnSrcLoc = SrcLoc { srcLocFileName = fn, srcLocLine = -1 }, warnType = t ,warnMessage = m } =
        putErrLn (fn ++ ": "  ++ msg t m)
    s Warning { warnSrcLoc = SrcLoc { srcLocFileName = fn, srcLocLine = l }, warnType = t ,warnMessage = m } =
        putErrLn (fn ++ ":" ++ pad 3 (show l) ++  " - "  ++ msg t m)
    die = (not $ null $ intersect (map warnType ws') fatal) && not (optKeepGoing options)

fatal = [
    "undefined-name",
    "ambiguous-name",
    "multiply-defined",
    "ambiguous-export",
    "unknown-import",
    "parse-error",
    "missing-dep",
    "invalid-decl",
    "invalid-assoc",
    "type-synonym-recursive",
    "type-synonym-partialap" ]

ignore = ["h98-emptydata"]

instance Show Warning where
    show  Warning { warnSrcLoc = sl, warnType = t, warnMessage = m } | sl == bogusASrcLoc =  msg t m
    show  Warning { warnSrcLoc = SrcLoc { srcLocFileName = fn, srcLocLine = l }, warnType = t ,warnMessage = m } =
         (fn ++ ":" ++ pad 3 (show l) ++  " - "  ++ msg t m)
msg "diagnostic" m = "Diagnostic: " ++ m
msg t m = (if t `elem` fatal then "Error: " else "Warning: ") ++ m

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
