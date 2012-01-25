{-------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)

        Module:                 Diagnostic

        Description:            Utilities for working with (error/otherwise)
                                diagnostics.

        Primary Authors:        Bryn Humberstone

        Notes:                  See the file License for license information

-------------------------------------------------------------------------------}

module FrontEnd.Diagnostic (
       Diagnostic(..), dumpDiagnostic,
       makeMsg,
       locMsg,
       locSimple,
       simpleMsg,
       typeError,
       TypeError (..),
       ) where

import Data.List as List(find)
import Data.Maybe (isJust)
import Data.Monoid
import FrontEnd.SrcLoc

--------------------------------------------------------------------------------

data TypeError
        = Unification String
        | BogusError
        | Failure String

typeError :: Monad m => TypeError -> [Diagnostic] -> m a
typeError err ds
   = fail $ "\n" ++
             "What:    " ++ whatStr ++ "\n" ++
             "Why:     " ++ whyStr ++ "\n" ++
             "Where:   " ++ dumpDiagnostic 3 ds
   where
   (whatStr, whyStr) =
        case err of
           Unification s -> ("type unification error", s)
           BogusError    -> ("bogus reason", "bogus reason")
           Failure s ->  ("failure", s)

data Diagnostic = Msg (Maybe SrcLoc) String
   deriving Show

{- Little helper functions for keeping good error contexts around -}
type Description = String

{- given a description, make a Diagnostic out of it -}
simpleMsg :: Description -> Diagnostic
simpleMsg description
   = Msg Nothing description

{- given a description and some data to be shown make a diagnostic -}
-- makeMsg :: PrettyShow a => Description -> a -> Diagnostic
makeMsg :: Description -> String -> Diagnostic
makeMsg description val
   = simpleMsg (description ++ "\n   " ++ val)

{- given a srcloc and a description, make a diagnostic -}
locSimple :: SrcLoc -> Description -> Diagnostic
locSimple loc desc = withASrcLoc loc (simpleMsg desc)

{- like locSimple but also takes data to be displayed -}
-- locMsg :: PrettyShow a => SrcLoc -> Description -> a -> Diagnostic
locMsg :: SrcLoc -> Description -> String -> Diagnostic
locMsg loc desc val = locSimple loc (desc ++ "\n   " ++ val)

{- take a diagnostic stack and a 'maxContext' and display the
   most recent maxContext number of lines from the stack -}
dumpDiagnostic :: Int -> [Diagnostic] -> String
dumpDiagnostic maxContext diagnostics
   = mostRecentASrcLoc ++ "\n"
      -- ++ (showDiagnostics . reverse . take maxContext $ diagnostics)
      ++ (showDiagnostics . take maxContext $ diagnostics)
   where
     hasASrcLoc diag
         = case diag of
                Msg maybeloc _ -> isJust maybeloc
           --   _ -> False

     mostRecentASrcLoc
         = case List.find hasASrcLoc diagnostics of
                Just (Msg (Just (SrcLoc fn line col)) _)
                    -> "on line " ++ show line ++ " in " ++ fn
                _ -> "no line information"

{- display an entire stack of diagnostics (it displays the top of
   the stack first, so most calls will have to reverse the stack
   before getting here -}
showDiagnostics :: [Diagnostic] -> String
showDiagnostics diags
    = case diags of
        [onlyOne] -> "The error was " ++ showDiag onlyOne
        _         -> showDiagnostics' diags
    where
    showDiagnostics' [] = ""
    showDiagnostics' (diag:diags)
       = case diags of
         --[] -> "\nSo the error was " ++ showDiag diag  -- innermost error
         [] -> showDiag diag  -- innermost error
         _  -> showDiag diag ++ "\n" ++ showDiagnostics' diags

    showDiag (Msg maybeLoc msg)
       = msg
         {- I think that all these line numbers are probably excessive -}
         ++ case maybeLoc of
              Just srcloc -> "\t\t{- on line " ++ show (srcLine srcloc) ++ " -}"  -- discreetly display line nums
              _ -> ""

srcLine :: SrcLoc -> Int
srcLine = srcLocLine

withASrcLoc :: SrcLoc -> Diagnostic -> Diagnostic
withASrcLoc loc x | loc == mempty = x
withASrcLoc loc (Msg _ description) = Msg (Just loc) description
