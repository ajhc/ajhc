module FrontEnd.Lex.Parse where

--import Control.Applicative
--import GenUtil
import Control.Monad.Error
import FrontEnd.HsPretty
import FrontEnd.HsSyn
import FrontEnd.Lex.Layout
import FrontEnd.Lex.Lexer
import FrontEnd.Lex.ParseMonad
import FrontEnd.Lex.Parser
import FrontEnd.Warning
import FrontEnd.SrcLoc
import Options
import PackedString
import System.Environment
import qualified FlagDump as FD

main = do
    as <- getArgs
    forM_ as $ \fn -> do
        hss <- readFile fn
        case scanner hss of
            Left s -> fail $ "Scanner: " ++ s
            Right s -> do
                wdump FD.Tokens $ do
                    putStrLn "-- scanned"
                    putStrLn $ unwords [ s | L _ _ s <- s ]
                s <- doLayout fn s
                wdump FD.Tokens $ do
                    putStrLn "-- after layout"
                    putStrLn $ unwords [ s | L _ _ s <- s ]
                opt <- getOptions
                case runP (parseModule s) opt of
                    (ws, ~(Just p)) -> do
                        processErrors ws
                        mapM_ print (hsModuleImports p)
                        mapM_ print (hsModuleDecls p)
                        putStrLn $ render (ppHsModule p)

parse :: Opt -> FilePath -> String -> IO HsModule
parse opt fp s = case scanner s of
    Left s -> fail s
    Right s -> do
        wdump FD.Tokens $ do
            putStrLn "-- scanned"
            putStrLn $ unwords [ s | L _ _ s <- s ]
        s <- doLayout fp s
        wdump FD.Tokens $ do
            putStrLn "-- after layout"
            putStrLn $ unwords [ s | L _ _ s <- s ]
        case runP (withSrcLoc bogusASrcLoc { srcLocFileName = packString fp } $ parseModule s) opt of
            (ws, ~(Just p)) -> do
                processErrors ws
                return p { hsModuleOpt = opt }
