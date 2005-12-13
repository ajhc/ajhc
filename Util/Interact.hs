-- This module contains routines to provide an interactive shell prompt and is
-- built on top of the readline library.

module Util.Interact(
    Interact(..),
    InteractCommand(..),
    beginInteraction,
    runInteraction,
    runInteractions,
    emptyInteract) where

import Char
import Control.Monad.Identity
import List
import qualified Data.Map as Map
import System
import System.Console.Readline
import System.Directory

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
    (":execfile", "run sequence of commands from a file"),
--    (":execfile!", "run sequence of commands from a file if it exists"),
    (":echo", "echo argument to screen"),
    (":addhist", "add argument to command line history"),
    (":help", "print help table")
    ]

extra_help = [
    ("!command", "run shell command")
    ]



basicParse :: String ->  Either (String,String) String
basicParse s = f s' where
    s' = reverse $ dropWhile isSpace (reverse $ dropWhile isSpace s)
    f (':':rs) = Left (':':map toLower as,dropWhile isSpace rest) where
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
    interactExpr :: Interact -> String -> IO Interact, -- ^ what to run on a bare expression
    interactRC   :: [String],               -- ^ commands to run at startup
    interactWords :: [String],              -- ^ list of words to autocomplete
    interactEcho :: Bool                    -- ^ whether to echo commands
    }

emptyInteract = Interact {
    interactPrompt = ">",
    interactCommands = [],
    interactSettables = [],
    interactVersion = "(none)",
    interactSet = Map.empty,
    interactExpr = \i s -> putStrLn ("Unknown Command: " ++ s) >> return i,
    interactRC = [],
    interactWords = [],
    interactEcho = False
    }

cleanupWhitespace s = reverse $ dropWhile isSpace (reverse $ dropWhile isSpace s)

runInteractions :: Interact -> [String] -> IO Interact
runInteractions act [] = return act
runInteractions act (x:xs) = do
    act' <- runInteraction act x
    runInteractions act' xs

-- | run a command as if typed at prompt

runInteraction :: Interact -> String -> IO Interact
runInteraction act s = do
    act <- runInteractions act { interactRC = [] } (interactRC act)
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
    case basicParse s of
        Right "" -> return act
        Right ('!':rest) -> System.system rest >> return act
        Right s -> do
            when (interactEcho act) $ putStrLn $ (interactPrompt act) ++ s
            act' <- interactExpr act act s
            return act'
        Left (cmd,arg) -> case fsts $ args cmd of
            [":quit"] -> putStrLn "Bye!" >> exitSuccess
            [":help"] -> putStrLn help_text >> return act
            [":version"] -> putStrLn (interactVersion act) >> return act
            [":echo"] -> putStrLn arg >> return act
            [":addhist"] -> addHistory arg >> return act
            [":cd"] -> catch (setCurrentDirectory arg) (\_ -> putStrLn $ "Could not change to directory: " ++ arg) >> return act
            [":pwd"] -> (catch getCurrentDirectory (\_ -> putStrLn "Could not get current directory." >> return "") >>= putStrLn)  >> return act
            [":set"] -> case simpleUnquote arg of
                [] -> showSet >> return act
                rs -> do
                    let ts = [ let (a,b) = span (/= '=') x in (cleanupWhitespace a,drop 1 b) | x <- rs ]
                    sequence_ [ putStrLn $ "Unknown option: " ++ a | (a,_) <- ts, a `notElem` interactSettables act]
                    return act { interactSet = Map.fromList [ x | x@(a,_) <- ts, a `elem` interactSettables act ] `Map.union` interactSet act }
            [":unset"] -> return act { interactSet = interactSet act Map.\\ Map.fromList [ (cleanupWhitespace rs,"") | rs <- simpleUnquote arg] }
            [":execfile"] -> do
                fc <- catch (readFile arg) (\_ -> putStrLn ("Could not read file: " ++ arg) >> return "")
                act <- runInteractions act { interactEcho = True } (lines fc)
                return act { interactEcho = False }
            [":execfile!"] -> do
                fc <- catch (readFile arg) (\_ -> return "")
                runInteractions act { interactEcho = True } (lines fc)
            [m] -> let [a] =  [ a | InteractCommand { commandName = n, commandAction = a } <-  interactCommands act, n == m] in do
                act' <- a act m arg
                return act'
            (_:_:_) -> putStrLn "Ambiguous command, possibilites are:" >> putStr  (unlines $ buildTableLL $ args cmd) >> return act
            [] -> (putStrLn $ "Unknown command (use :help for help): " ++ cmd)  >> return act


-- | begin interactive interaction

beginInteraction :: Interact -> IO ()
beginInteraction act = do
    let commands' = commands ++ [ (n,h) | InteractCommand { commandName = n, commandHelp = h } <- interactCommands act ]
        args s =  [ bb | bb@(n,_) <- commands', s `isPrefixOf` n ]
        expand s = snub $ fsts (args s) ++ filter (isPrefixOf s) (interactSettables act ++ interactWords act)
    s <- readLine (interactPrompt act) (return . expand)
    act' <- runInteraction act s
    beginInteraction act'


