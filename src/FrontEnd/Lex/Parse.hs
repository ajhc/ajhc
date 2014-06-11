module FrontEnd.Lex.Parse(parseM,parse,parseStmt) where

import Util.Std

import FrontEnd.HsSyn
import FrontEnd.Lex.Layout
import FrontEnd.Lex.Layout2 as L2
import FrontEnd.Lex.Lexer
import FrontEnd.Lex.ParseMonad
import FrontEnd.Warning
import Options
import PackedString
import qualified FlagDump as FD
import qualified FrontEnd.Lex.Parser as P

parseM :: MonadWarn m => Opt -> FilePath -> String -> m (Maybe HsModule)
parseM opt fp s = case scanner opt s of
    Left s -> fail s
    Right s -> do
        s <- doLayout opt fp s
        case runP (withSrcLoc bogusASrcLoc { srcLocFileName = packString fp } $ P.parseModule s) opt of
            (ws, ~(Just p)) -> do
                mapM_ addWarning ws
                if null ws
                    then return $ Just p { hsModuleOpt = opt }
                    else return Nothing

parse :: Opt -> FilePath -> String -> IO HsModule
parse opt fp s = case scanner opt s of
    Left s -> fail s
    Right s -> do
        let pp = preprocessLexemes opt fp s
        wdump FD.Tokens $ do
            putStrLn "-- scanned"
            putStrLn $ unwords [ s | L _ _ s <- s ]
            putStrLn "-- preprocessed"
            forM_ pp $ \c -> case c of
                Token (L _ _ s) -> putStr s >> putStr " "
                TokenNL n -> putStr ('\n':replicate (n - 1) ' ')
                TokenVLCurly _ i -> putStr $ "{" ++ show (i - 1) ++ " "
            putStrLn ""
        laidOut <- doLayout opt fp s
        wdump FD.Tokens $ do
            putStrLn "-- after layout"
            putStrLn $ unwords [ s | L _ _ s <- laidOut ]

            putStrLn "-- after layout2"
            let --f n (L _ _ "{":xs) = putStr (" {\n" ++ replicate n ' ') >> f (n + 2) xs
                --f n (L _ _ ";":xs) = putStr (" ;\n" ++ replicate n ' ') >> f n xs
                --f n (L _ _ "}":xs) = putStr (" }\n" ++ replicate (n - 2) ' ') >> f (n - 2) xs
                f n (L _ _ s:xs) = putStr (' ':s) >> f n xs
                f n [] = return ()
            f (0::Int) (L2.layout pp)
--            putStrLn $ unwords [ s | L _ _ s <- L2.layout pp ]

        case runP (withSrcLoc bogusASrcLoc { srcLocFileName = packString fp } $ P.parseModule (L2.layout pp)) opt of
        --case runP (withSrcLoc bogusASrcLoc { srcLocFileName = packString fp } $ P.parseModule laidOut) opt of
            (ws, ~(Just p)) -> do
                processErrors ws
                return p { hsModuleOpt = opt }

parseStmt :: (Applicative m,MonadWarn m) => Opt -> FilePath -> String -> m HsStmt
parseStmt opt fp s = case scanner opt s of
    Left s -> fail s
    Right s -> do
        s <- doLayout opt fp s
        case runP (withSrcLoc bogusASrcLoc { srcLocFileName = packString fp } $ P.parseStmt s) opt of
            (ws, ~(Just p)) -> do
                mapM_ addWarning ws
                return p
