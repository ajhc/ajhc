-- #hide
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
		ParseMode(..), defaultParseMode,
		runParserWithMode, runParser,
		getSrcLoc, pushCurrentContext, popContext,thenP,returnP,
		-- * Lexing
		Lex(runL), getInput, discard, lexNewline, lexTab, lexWhile,
		alternative, checkBOL, setBOL, startToken, getOffside,
		pushContextL, popContextL
	) where

import FrontEnd.SrcLoc
import Warning

-- | The result of a parse.
data ParseResult a
	= ParseOk [Warning] a	-- ^ The parse succeeded, yielding a value and a set of warnings.
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
data ParseState = ParseState { psLexContext :: [LexContext], psWarnings :: [Warning] }
    deriving(Show)

indentOfParseState :: ParseState -> Int
indentOfParseState ParseState { psLexContext = (Layout n:_) } = n
indentOfParseState _            = 0

emptyParseState = ParseState { psLexContext = [], psWarnings = [] }

-- | Static parameters governing a parse.
-- More to come later, e.g. literate mode, language extensions.

data ParseMode = ParseMode {
                -- | original name of the file being parsed
		parseFilename :: String
		}

-- | Default parameters for a parse,
-- currently just a marker for an unknown filename.

defaultParseMode :: ParseMode
defaultParseMode = ParseMode {
		parseFilename = "<unknown>"
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

runParserWithMode :: ParseMode -> P a -> String -> ParseResult a
runParserWithMode mode (P m) s = case m s 0 1 start emptyParseState mode of
	Ok s a -> ParseOk (psWarnings s) a
	Failed loc msg -> ParseFailed loc msg
    where start = SrcLoc {
		srcLocFileName = parseFilename mode,
		srcLocLine = 1,
		srcLocColumn = 1
	}

runParser :: P a -> String -> ParseResult a
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
	loc <- getSrcLoc
	indent <- currentIndent
	pushContext (Layout (max (indent+1) (srcLocColumn loc)))

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

-- Monad for lexical analysis:
-- a continuation-passing version of the parsing monad

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

-- | Discard the next character, which must be a newline.

lexNewline :: Lex a ()
lexNewline = Lex $ \cont -> P $ \(_:r) _x y -> runP (cont ()) r 1 (y+1)

-- | Discard the next character, which must be a tab.

lexTab :: Lex a ()
lexTab = Lex $ \cont -> P $ \(_:r) x -> runP (cont ()) r (nextTab x)

nextTab :: Int -> Int
nextTab x = x + (tAB_LENGTH - (x-1) `mod` tAB_LENGTH)

tAB_LENGTH :: Int
tAB_LENGTH = 8

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
startToken = Lex $ \cont -> P $ \s x y _ stk mode ->
	let loc = SrcLoc {
		srcLocFileName = parseFilename mode,
		srcLocLine = y,
		srcLocColumn = x
	} in
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
