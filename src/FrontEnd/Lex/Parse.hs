module FrontEnd.Lex.Parse where

import FrontEnd.HsSyn
import FrontEnd.Lex.Layout
import FrontEnd.Lex.Lexer
import FrontEnd.Lex.ParseMonad
import FrontEnd.Lex.Parser
import FrontEnd.SrcLoc
import FrontEnd.Warning
import Options
import PackedString
import qualified FlagDump as FD

parse :: Opt -> FilePath -> String -> IO HsModule
parse opt fp s = case scanner opt s of
    Left s -> fail s
    Right s -> do
        wdump FD.Tokens $ do
            putStrLn "-- scanned"
            putStrLn $ unwords [ s | L _ _ s <- s ]
        s <- doLayout opt fp s
        wdump FD.Tokens $ do
            putStrLn "-- after layout"
            putStrLn $ unwords [ s | L _ _ s <- s ]
            putStrLn $ unwords [ show t ++ ":" ++s | L _ t s <- s ]
        case runP (withSrcLoc bogusASrcLoc { srcLocFileName = packString fp } $ parseModule s) opt of
            (ws, ~(Just p)) -> do
                processErrors ws
                return p { hsModuleOpt = opt }
