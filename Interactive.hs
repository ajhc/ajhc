module Interactive(Interactive.interact) where

import Data.Version
import IO(stdout)
import List(sort)
import Maybe
import qualified Data.Map as Map
import System.Info
import Text.Regex
import Text.Regex.Posix(regcomp,regExtended)


import Doc.DocLike
import Doc.Pretty
import GenUtil
import Ho
import HsSyn
import Name.Name
import Options
import Util.Interact
import Version

printDoc doc = do
    displayIO stdout (renderPretty 0.9 80 doc)
    putStrLn ""

{-
 'f' - normal value
 'C' - data constructor
 'T' - type constructor
 'L' - class
 '?' - odd
-}

nameTag :: NameType -> Char
nameTag TypeConstructor = 'T'
nameTag DataConstructor = 'C'
nameTag ClassName = 'L'
nameTag Val = 'f'
nameTag _ = '?'

interact :: Ho -> IO ()
interact ho = beginInteraction emptyInteract { interactSettables = ["prog", "args"], interactVersion = versionString, interactCommands = commands } where
    commands = [cmd_mods,cmd_grep]
    cmd_mods = InteractCommand { commandName = ":mods", commandHelp = "mods currently loaded modules", commandAction = do_mods }
    do_mods act _ _ = do
        printDoc $ fillSep (map tshow $ Map.keys $  hoExports ho)
        return act
    cmd_grep = InteractCommand { commandName = ":grep", commandHelp = "show names matching a regex", commandAction = do_grep }
    do_grep act _ "" = do
        putStrLn ":grep <regex>"
        return act
    do_grep act _ arg = do
        rx <- catch ( Just `fmap` regcomp arg regExtended) (\_ -> return Nothing)
        case rx of
            Nothing -> putStrLn $ "Invalid regex: " ++ arg
            Just rx -> mapM_ putStrLn $ sort [ nameTag (nameType v):' ':show v | v <- Map.keys (hoDefs ho), isJust (matchRegex rx (show v)) ]
        return act


