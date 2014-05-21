import Control.Applicative
import Control.Monad.Error
import FrontEnd.HsPretty
import FrontEnd.HsSyn
import FrontEnd.Lex.Layout
import FrontEnd.Lex.Lexer
import FrontEnd.Lex.Parser
import GenUtil
import Options
import System.Environment
import Version.Version(versionString)

import Util.Interact

initialInteract = emptyInteract {
    interactSettables = ["prog", "args"],
    interactVersion = versionString,
    interactCommands = [], -- commands,
    interactWords = [],  --map (show . fst ) $ stateImports isStart,
    interactHistFile = Just ".jhci-np-hist",
    interactComment = Just "--",
    interactExpr = do_expr
    }
do_expr :: Interact -> String -> IO Interact
do_expr iact st = do
    case scanner st of
        Left s -> putStrLn $ "Scanner: " ++ s
        Right s -> do
            print s
            case parseStmt s of
                Left s -> putStrLn $ "Parse: " ++ s
                Right p -> print p
    return iact
main = do
    as <- getArgs
    case as of
        [] -> beginInteraction initialInteract
        _ -> do
            c <- getArgContents
            case scanner c of
                Left s -> fail $ "Scanner: " ++ s
                Right s -> do
                    putStrLn "-- scanned"
                    putStrLn $ unwords [ s | L _ _ s <- s ]
                    s <- doLayout s
                    putStrLn "-- after layout"
                    print s
                    putStrLn $ unwords [ s | L _ _ s <- s ]
                    case parseModule s of
                        Left s -> fail $ "Parse: " ++ s
                        Right p -> do
                            mapM_ print (hsModuleImports p)
                            mapM_ print (hsModuleDecls p)
                            putStrLn $ render (ppHsModule p)
