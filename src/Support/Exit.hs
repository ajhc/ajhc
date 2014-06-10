module Support.Exit(exitSuccess, exitFailure, exitWith, module Support.Exit, module Util.ExitCodes) where

import System.Exit
import Util.ExitCodes

exitCodeLexError = ExitFailure 2
exitCodeParseError = ExitFailure 3
exitCodeTypeError = ExitFailure 4
