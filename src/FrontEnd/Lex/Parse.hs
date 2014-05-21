--module FrontEnd.Lex.Parse where

--import Control.Applicative
import Control.Monad.Error
import FrontEnd.HsPretty
import FrontEnd.HsSyn
import FrontEnd.Lex.Layout
import FrontEnd.Lex.Lexer
import FrontEnd.Lex.Parser
import FrontEnd.Lex.ParseMonad
import FrontEnd.Warning
--import GenUtil
import Options
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
