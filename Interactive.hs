module Interactive(Interactive.interact) where

import IO(stdout)
import List(sort)
import Maybe
import qualified Data.Map as Map
import Text.Regex
import Text.Regex.Posix(regcomp,regExtended)


import DataConstructors
import FrontEnd.HsParser(parseHsStmt)
import FrontEnd.ParseMonad
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import GenUtil
import Ho
import HsSyn
import Name.Name
import Options
import qualified Text.PrettyPrint.HughesPJ as PP
import Util.Interact

printDoc doc = do
    displayIO stdout (renderPretty 0.9 80 doc)
    putStrLn ""

grep_opts = [
 "f - match normal value",
 "C - match data constructor",
 "T - match type constructor",
 "L - match class"
 ]

nameTag :: NameType -> Char
nameTag TypeConstructor = 'T'
nameTag DataConstructor = 'C'
nameTag ClassName = 'L'
nameTag Val = 'f'
nameTag _ = '?'

interact :: Ho -> IO ()
interact ho = go where
    go = do
        putStrLn "--------------------------------------------------------------"
        putStrLn "Welcome to the jhc interactive experience. use :help for help."
        putStrLn versionString
        putStrLn "--------------------------------------------------------------"
        beginInteraction emptyInteract { interactSettables = ["prog", "args"], interactVersion = versionString, interactCommands = commands, interactExpr = do_expr }
    dataTable = hoDataTable ho
    commands = [cmd_mods,cmd_grep]
    cmd_mods = InteractCommand { commandName = ":mods", commandHelp = "mods currently loaded modules", commandAction = do_mods }
    do_mods act _ _ = do
        printDoc $ fillSep (map tshow $ Map.keys $  hoExports ho)
        return act
    cmd_grep = InteractCommand { commandName = ":grep", commandHelp = "show names matching a regex", commandAction = do_grep }
    do_grep act _ "" = do
        putStrLn ":grep [options] <regex>"
        putStrLn "Valid options:"
        putStr $ unlines grep_opts
        return act
    do_grep act _ arg = do
        let (opt,reg) = case simpleUnquote arg of
                [x] -> ("TCLf",x)
                xs -> f "" xs where
            f opt [x] = (opt,x)
            f opt ~(x:xs) = f (x ++ opt) xs
            f _ _ = undefined
        rx <- catch ( Just `fmap` regcomp reg regExtended) (\_ -> return Nothing)
        case rx of
            Nothing -> putStrLn $ "Invalid regex: " ++ arg
            Just rx -> mapM_ putStrLn $ sort [ nameTag (nameType v):' ':show v <+> "::" <+> ptype v  | v <- Map.keys (hoDefs ho), isJust (matchRegex rx (show v)), nameTag (nameType v) `elem` opt ]
        return act
    ptype k | Just r <- Map.lookup k (hoAssumps ho) = show (pprint r:: PP.Doc)
    ptype x = pprintTypeOfCons dataTable x
    do_expr :: Interact -> String -> IO Interact
    do_expr act s = case parseStmt s of
        Left m -> putStrLn m >> return act
        Right e -> putStrLn (show e) >> return act


parseStmt ::  Monad m => String -> m HsStmt
parseStmt s = case runParserWithMode ParseMode { parseFilename = "(jhci)" } parseHsStmt  s  of
                      ParseOk e -> return e
                      ParseFailed sl err -> fail $ show sl ++ ": " ++ err


