-- This module contains routines to provide an interactive shell prompt and is
-- built on top of the readline library.

module Util.Interact(Interact(..),InteractCommand(..),beginInteraction,emptyInteract) where

import Char
import Control.Monad.Identity
import Data.Version
import List
import qualified Data.Map as Map
import System
import System.Console.Readline
import System.Directory
import System.Info

import GenUtil


readLine :: String -> (String -> IO [String]) -> IO String
readLine prompt tabExpand =  do
    setCompletionEntryFunction (Just (\s -> tabExpand s))
    s <- readline prompt
    case s of
        Nothing -> putStrLn "Bye!" >> exitSuccess
        Just cs | all isSpace cs -> return ""
        Just s -> addHistory s >> return s


--simpleCommand :: String -> IO (Maybe String)

commands = [
    (":quit","quit interactive session"),
    (":version","print out version number"),
    (":cd", "change directory to argument"),
    (":pwd", "show current directory"),
    (":set", "set options"),
    (":unset", "unset options"),
    (":help", "print help table")
    ]

extra_help = [
    ("!command", "run shell command")
    ]



basicParse :: String ->  Either (String,String) String
basicParse s = f s' where
    s' = reverse $ dropWhile isSpace (reverse $ dropWhile isSpace s)
    f (':':rs) = Left (':':as,dropWhile isSpace rest) where
        (as,rest) = span isAlpha rs
    f _ = Right s'

data InteractCommand = InteractCommand {
    commandName :: String,
    commandHelp :: String,
    commandAction :: Interact -> String -> String -> IO Interact
    }

data Interact = Interact {
    interactPrompt :: String,               -- ^ the prompt to use
    interactCommands :: [InteractCommand],  -- ^ a list of commands
    interactSettables :: [String],          -- ^ possible things that may be set
    interactVersion :: String,              -- ^ version string to print
    interactSet :: Map.Map String String,   -- ^ vars that are actually set
    interactExpr :: Interact -> String -> IO Interact -- ^ what to run on a bare expression
    }

emptyInteract = Interact {
    interactPrompt = ">",
    interactCommands = [],
    interactSettables = [],
    interactVersion = "(none)",
    interactSet = Map.empty,
    interactExpr = \i s -> putStrLn ("Unknown Command: " ++ s) >> return i
    }

cleanupWhitespace s = reverse $ dropWhile isSpace (reverse $ dropWhile isSpace s)

beginInteraction :: Interact -> IO ()
beginInteraction act = do
    let commands' = commands ++ [ (n,h) | InteractCommand { commandName = n, commandHelp = h } <- interactCommands act ]
        help_text = unlines $ buildTableLL (commands' ++ extra_help)
    let args s =  [ bb | bb@(n,_) <- commands', s `isPrefixOf` n ]
        expand s = fsts (args s) ++ filter (isPrefixOf s) (interactSettables act)

    let showSet
         | null $ interactSettables act = putStrLn "Nothing may be set"
         | otherwise  = do
            let set = [ "  " ++ if null b then a else a ++ "=" ++ b | (a,b) <- Map.toList $ interactSet act]
                setable = [ "  " ++ a | a <- sort $ interactSettables act, not $ a `Map.member` interactSet act]
            when (not $ null set) $ putStrLn "Set options:" >> putStr (unlines set)
            when (not $ null setable) $ putStrLn "Setable options:" >> putStr (unlines setable)
    s <- readLine (interactPrompt act) (return . expand)
    case basicParse s of
        Right "" -> beginInteraction act
        Right ('!':rest) -> System.system rest >> return ()
        Right s -> do
            act' <- interactExpr act act s
            beginInteraction act'
        Left (cmd,arg) -> case fsts $ args cmd of
            [":quit"] -> putStrLn "Bye!" >> exitSuccess
            [":help"] -> putStrLn help_text
            [":version"] -> putStrLn (interactVersion act)
            [":cd"] -> catch (setCurrentDirectory arg) (\_ -> putStrLn $ "Could not change to directory: " ++ arg)
            [":pwd"] -> catch getCurrentDirectory (\_ -> putStrLn "Could not get current directory." >> return "") >>= putStrLn
            [":set"] -> case simpleUnquote arg of
                [] -> showSet
                rs -> do
                    let ts = [ let (a,b) = span (/= '=') x in (cleanupWhitespace a,drop 1 b) | x <- rs ]
                    sequence_ [ putStrLn $ "Unknown option: " ++ a | (a,_) <- ts, a `notElem` interactSettables act]
                    beginInteraction act { interactSet = Map.fromList [ x | x@(a,_) <- ts, a `elem` interactSettables act ] `Map.union` interactSet act }
            [":unset"] -> beginInteraction act { interactSet = interactSet act Map.\\ Map.fromList [ (cleanupWhitespace rs,"") | rs <- simpleUnquote arg] }
            [m] -> let [a] =  [ a | InteractCommand { commandName = n, commandAction = a } <-  interactCommands act, n == m] in do
                act' <- a act m arg
                beginInteraction act'
            (_:_:_) -> putStrLn "Ambiguous command, possibilites are:" >> putStr  (unlines $ buildTableLL $ args cmd)
            [] -> putStrLn $ "Unknown command (use :help for help): " ++ cmd
    beginInteraction act

