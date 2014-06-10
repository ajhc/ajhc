module Support.Exit(exitSuccess, exitFailure, exitWith, module Support.Exit, module Util.ExitCodes,ExitCode(..)) where

import System.Exit
import Util.ExitCodes

exitCodeFailure = ExitFailure 1
exitCodeLexError = ExitFailure 2
exitCodeParseError = ExitFailure 3
exitCodeCodeError = ExitFailure 4
exitCodeTypeError = ExitFailure 5
