{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.ParseMonad
-- Copyright   :  (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Monads for the Haskell parser and lexer.
--
-----------------------------------------------------------------------------

module FrontEnd.ParseMonad(
		-- * Parsing
		P, ParseResult(..), atSrcLoc, LexContext(..),
		ParseMode(..),
                parseModeOptions,
		runParserWithMode, runParser,
		getSrcLoc, setSrcLoc, pushCurrentContext, popContext,thenP,returnP,
		-- * Lexing
		Lex(runL), getInput, discard, lexNewline, lexTab, lexWhile,
		alternative, checkBOL, setBOL, startToken, getOffside,
		pushContextL, popContextL, lexParseMode,
                pullCtxtFlag, setFlagDo
	) where

import qualified Data.Set as Set

import Control.Monad
import Data.Monoid
import Data.Functor
import FrontEnd.SrcLoc
import FrontEnd.Warning
import Options
import qualified FlagOpts as FO
import qualified Control.Applicative as A

-- | The result of a parse.
data ParseResult a
	= ParseOk a	-- ^ The parse succeeded, yielding a value and a set of warnings.
	| ParseFailed SrcLoc String
				-- ^ The parse failed at the specified
				-- source location, with an error message.
	deriving Show

-- internal version
data ParseStatus a = Ok ParseState a | Failed SrcLoc String
	deriving Show

data LexContext = NoLayout | Layout Int
	deriving (Eq,Ord,Show)

--type ParseState = [LexContext]
data ParseState = ParseState {
    psLexContext :: [LexContext],
    psWarnings :: [Warning],
    psInDo :: !Bool,
    psForceClose :: !Bool
    } deriving(Show)


instance Functor ParseResult where
    fmap f (ParseOk x) = ParseOk (f x)
    fmap _ (ParseFailed x y) = ParseFailed x y

instance A.Applicative ParseResult where
  pure = ParseOk
  ParseOk f <*> x = f <$> x
  ParseFailed loc msg <*> _ = ParseFailed loc msg

instance Monad ParseResult where
  return = A.pure
  ParseOk x         >>= f = f x
  ParseFailed loc msg >>= _ = ParseFailed loc msg

instance Monoid m => Monoid (ParseResult m) where
  mempty = ParseOk mempty
  ParseOk x `mappend` ParseOk y = ParseOk $ x `mappend` y
  ParseOk x `mappend` err       = err
  err       `mappend` _         = err -- left-biased

indentOfParseState :: ParseState -> Int
indentOfParseState ParseState { psLexContext = (Layout n:_) } = n
indentOfParseState _            = 0

emptyParseState = ParseState { psLexContext = [], psWarnings = [], psForceClose = False, psInDo = False }

-- | Static parameters governing a parse.
-- More to come later, e.g. literate mode, language extensions.

data ParseMode = ParseMode {
                -- | original name of the file being parsed
		parseFilename      :: String,
                parseFFI           :: Bool,
                parseUnboxedValues :: Bool,
                parseUnboxedTuples :: Bool
		}

-- | Default parameters for a parse,
-- currently just a marker for an unknown filename.

defaultParseMode :: ParseMode
defaultParseMode = ParseMode {
		parseFilename = "<unknown>",
                parseFFI = False,
                parseUnboxedValues = False,
                parseUnboxedTuples = False
		}

parseModeOptions options = defaultParseMode {
    parseUnboxedTuples = FO.UnboxedTuples `Set.member` optFOptsSet options || FO.UnboxedValues `Set.member` optFOptsSet options,
    parseUnboxedValues = FO.UnboxedValues `Set.member` optFOptsSet options,
    parseFFI = FO.Ffi `Set.member` optFOptsSet options
    }

-- | Monad for parsing


newtype P a = P { runP ::
		        String		-- input string
		     -> Int		-- current column
		     -> Int		-- current line
		     -> SrcLoc		-- location of last token read
		     -> ParseState	-- layout info.
		     -> ParseMode	-- parse parameters
		     -> ParseStatus a
		}

runParserWithMode :: ParseMode -> P a -> String -> ([Warning],ParseResult a)
runParserWithMode mode (P m) s = case m s 0 1 start emptyParseState mode of
	Ok s a -> (psWarnings s,ParseOk a)
	Failed loc msg -> ([],ParseFailed loc msg)
    where start = SrcLoc {
		srcLocFileName = parseFilename mode,
		srcLocLine = 1,
		srcLocColumn = 1
	}

runParser :: P a -> String -> ([Warning],ParseResult a)
runParser = runParserWithMode defaultParseMode

instance Monad P where
	return a = P $ \_i _x _y _l s _m -> Ok s a
	P m >>= k = P $ \i x y l s mode ->
		case m i x y l s mode of
		    Failed loc msg -> Failed loc msg
		    Ok s' a -> runP (k a) i x y l s' mode
	fail s = P $ \_r _col _line loc _stk _m -> Failed loc s

returnP :: a -> P a
returnP = return
thenP :: P a -> (a -> P b) -> P b
thenP = (>>=)

atSrcLoc :: P a -> SrcLoc -> P a
P m `atSrcLoc` loc = P $ \i x y _l -> m i x y loc

--getSrcLoc :: P SrcLoc

instance MonadSrcLoc P where
    getSrcLoc = P $ \_i _x _y l s _m -> Ok s l

instance MonadWarn P where
    addWarning w = P $ \_i _x _y _l s _m -> Ok s { psWarnings = w:psWarnings s } ()

-- Enter a new layout context.  If we are already in a layout context,
-- ensure that the new indent is greater than the indent of that context.
-- (So if the source loc is not to the right of the current indent, an
-- empty list {} will be inserted.)

pushCurrentContext :: P ()
pushCurrentContext = do
	lc <- getSrcLoc
	indent <- currentIndent
        let loc = srcLocColumn lc
        dob <- pullDoStatus
        when (if dob then loc < indent else loc <= indent) pushCtxtFlag
	pushContext (Layout loc)

currentIndent :: P Int
currentIndent = P $ \_r _x _y loc stk _mode -> Ok stk (indentOfParseState stk)

pushContext :: LexContext -> P ()
pushContext ctxt =
--trace ("pushing lexical scope: " ++ show ctxt ++"\n") $
	P $ \_i _x _y _l s _m -> Ok s { psLexContext = ctxt:psLexContext s } ()

popContext :: P ()
popContext = P $ \_i _x _y _l stk _m ->
      case psLexContext stk of
   	(_:s) -> --trace ("popping lexical scope, context now "++show s ++ "\n") $
            Ok stk { psLexContext = s } ()
        []    -> error "Internal error: empty context in popContext"

pullCtxtFlag :: Lex a Bool
pullCtxtFlag = Lex $ \cont -> P $ \r x y loc s ->
        runP (cont $ psForceClose s) r x y loc s { psForceClose = False }

pushCtxtFlag :: P ()
pushCtxtFlag =
    P $ \_i _x _y _l s _m -> case psForceClose s of
        False -> Ok s { psForceClose = True } ()
        _     -> error "Internal error: context flag already pushed"

pullDoStatus :: P Bool
pullDoStatus = P $ \_i _x _y _l s _m -> Ok s { psInDo = False } (psInDo s)

setFlagDo :: Lex a ()
setFlagDo = Lex $ \cont -> P $ \r x y loc s ->
        runP (cont ()) r x y loc s { psInDo = True }

-- Monad for lexical analysis:
-- a continuation-passing version of the parsing monad
--
newtype Lex r a = Lex { runL :: (a -> P r) -> P r }

instance Monad (Lex r) where
	return a = Lex $ \k -> k a
	Lex v >>= f = Lex $ \k -> v (\a -> runL (f a) k)
	Lex v >> Lex w = Lex $ \k -> v (\_ -> w k)
	fail s = Lex $ \_ -> fail s

instance MonadWarn (Lex r) where
    addWarning w = Lex $ \k -> addWarning w >> k ()
instance MonadSrcLoc (Lex r) where
    getSrcLoc = Lex $ \k -> getSrcLoc >>= k

-- Operations on this monad

getInput :: Lex r String
getInput = Lex $ \cont -> P $ \r -> runP (cont r) r

-- | Discard some input characters (these must not include tabs or newlines).

discard :: Int -> Lex r ()
discard n = Lex $ \cont -> P $ \r x -> runP (cont ()) (drop n r) (x+n)


setSrcLoc :: SrcLoc -> Lex a ()
setSrcLoc srcloc = Lex $ \cont -> P $ \r x l _ -> runP (cont ()) r x l srcloc

-- | Discard the next character, which must be a newline.

lexNewline :: Lex a ()
lexNewline = Lex $ \cont -> P $ \(_:r) _x y loc -> runP (cont ()) r 1 (y+1) loc { srcLocLine = srcLocLine loc + 1 }

-- | Discard the next character, which must be a tab.

lexTab :: Lex a ()
lexTab = Lex $ \cont -> P $ \(_:r) x -> runP (cont ()) r (nextTab x)

nextTab :: Int -> Int
nextTab x = x + (tAB_LENGTH - (x-1) `mod` tAB_LENGTH)

tAB_LENGTH :: Int
tAB_LENGTH = 8

lexParseMode :: Lex a ParseMode
lexParseMode = Lex $ \cont -> P $ \r x y z s m -> runP (cont m) r x y z s m

-- Consume and return the largest string of characters satisfying p

lexWhile :: (Char -> Bool) -> Lex a String
lexWhile p = Lex $ \cont -> P $ \r x ->
	let (cs,rest) = span p r in
	runP (cont cs) rest (x + length cs)

-- An alternative scan, to which we can return if subsequent scanning
-- is unsuccessful.

alternative :: Lex a v -> Lex a (Lex a v)
alternative (Lex v) = Lex $ \cont -> P $ \r x y ->
	runP (cont (Lex $ \cont' -> P $ \_r _x _y ->
		runP (v cont') r x y)) r x y

-- The source location is the coordinates of the previous token,
-- or, while scanning a token, the start of the current token.

-- col is the current column in the source file.
-- We also need to remember between scanning tokens whether we are
-- somewhere at the beginning of the line before the first token.
-- This could be done with an extra Bool argument to the P monad,
-- but as a hack we use a col value of 0 to indicate this situation.

-- Setting col to 0 is used in two places: just after emitting a virtual
-- close brace due to layout, so that next time through we check whether
-- we also need to emit a semi-colon, and at the beginning of the file,
-- by runParser, to kick off the lexer.
-- Thus when col is zero, the true column can be taken from the loc.

checkBOL :: Lex a Bool
checkBOL = Lex $ \cont -> P $ \r x y loc ->
		if x == 0 then runP (cont True) r (srcLocColumn loc) y loc
			else runP (cont False) r x y loc

setBOL :: Lex a ()
setBOL = Lex $ \cont -> P $ \r _ -> runP (cont ()) r 0

-- Set the loc to the current position

startToken :: Lex a ()
startToken = Lex $ \cont -> P $ \s x y oloc stk mode ->
	let loc = oloc { srcLocColumn = x } in
	runP (cont ()) s x y loc stk mode

-- Current status with respect to the offside (layout) rule:
-- LT: we are to the left of the current indent (if any)
-- EQ: we are at the current indent (if any)
-- GT: we are to the right of the current indent, or not subject to layout

getOffside :: Lex a Ordering
getOffside = Lex $ \cont -> P $ \r x y loc stk ->
		runP (cont (compare x (indentOfParseState stk))) r x y loc stk

pushContextL :: LexContext -> Lex a ()
pushContextL ctxt = Lex $ \cont -> P $ \r x y loc stk ->
		runP (cont ()) r x y loc stk { psLexContext = ctxt:psLexContext stk }

popContextL :: String -> Lex a ()
popContextL fn = Lex $ \cont -> P $ \r x y loc stk -> case psLexContext stk of
		(_:ctxt) -> runP (cont ()) r x y loc stk { psLexContext = ctxt }
		[]       -> error ("Internal error: empty context in " ++ fn)



{-
-- ---------------------------------------------------------------------------
-- Construct a parse error

srcParseErr
  :: String       -- current buffer (placed just after the last token)
  -> Int                -- length of the previous token
  -> Message
srcParseErr buf len
  = hcat [ if null token
         then ptext SLIT("parse error (possibly incorrect indentation)")
         else hcat [ptext SLIT("parse error on input "),
                char '`', text token, char '\'']
    ]
  where token = lexemeToString (stepOnBy (-len) buf) len

-- Report a parse failure, giving the span of the previous token as
-- the location of the error.  This is the entry point for errors
-- detected during parsing.
srcParseFail :: P a
srcParseFail = P $ \buf _ _ last_loc _ _ ->
    Failed last_loc (srcParseErr buf len)

-- A lexical error is reported at a particular position in the source file,
-- not over a token range.  TODO: this is slightly wrong, because we record
-- the error at the character position following the one which caused the
-- error.  We should somehow back up by one character.
--lexError :: String -> P a
--lexError str = do
--  loc <- getSrcLoc
--  i@(end,_) <- getInput
--  failLocMsgP loc end str


-}
