{
{-# OPTIONS_GHC -w -XNoOverloadedStrings #-} {- -*- Haskell -*- -}
module FrontEnd.Lex.Lexer (scanner,Lexeme(..),LexemeClass(..),AlexPosn(..)) where

import Control.Monad
import Data.Word (Word8)
import FrontEnd.Lexer(Token(..))
import FrontEnd.SrcLoc
import Name.Name
import Options
import qualified Data.Bits
import qualified FlagOpts as FO
}

-- %wrapper "monadUserState"

$unispace  = [\xa0]
$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]
$unispecial = [→←∷‥⇒∀∃]
$trailing  = [₀₁₂₃₄₅₆₇₈₉⁰¹²³⁴⁵⁶⁷⁸⁹₍₎⁽⁾₊₋]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']   -- "

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']   -- "

$octit	   = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]
$ws        = [$unispace $white]

@reservedid =
	case|class|data|default|deriving|do|else|if|
	import|in|infix|infixl|infixr|instance|let|module|newtype|
	of|then|type|where|_

-- reserved when certain extensions enabled
@extreservedid =  foreign|forall|exists|kind|alias|prefixx|prefixy|family|closed

-- sometimes
@specialid = as|hiding|qualified

@reservedop =
	".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"

@varid  = $small $idchar* $trailing*
@conid  = $large $idchar* $trailing*
@varsym = $symbol $symchar* $trailing*
@consym = \: $symchar* $trailing*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
	 | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
	 | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
	 | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]                                                -- "
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal | \n)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap                        -- "
@ws      = $ws+
@integer = @decimal | 0[oO] @octal | 0[xX] @hexadecimal
@stringlit = \" @string* \"  -- "

haskell :-

--------------------
-- handle whitespace
--------------------

<0>  ^"#!" .*              { begin hs }
<0>  ""                    { begin hs }
<hs> $ws+		   ;
<hs> "--"\-*[^$symbol].*   ;
<hs> "--"\-*$	           ;
<hs> ^"#line " .*          ;

-- Handle CPP style line pragmas
-- <hs> ^"#line " @ws                { mkJL LPragmaStart "LINE" `andBegin` line_pragma }
-- <hs> ^"# " / @decimal          { mkJL LPragmaStart "LINE" `andBegin` line_pragma }
-- <line_pragma> $white*$          { mkJL LSpecial "#-}" `andBegin` 0 }

"{-#" @ws "OPTIONS"             { begin discard_pragma }
"{-#" @ws "LANGUAGE"            { begin discard_pragma }
"{-#" @ws "GHC"                 { begin discard_pragma }
<discard_pragma> "#-}"          { begin hs }
<discard_pragma> .              ;
"{-#"                           { mkL LSpecial }
"#-}"                           { mkL LSpecial }

"{-"				{ nested_comment }

-----------------------
-- reserved/special ops
-----------------------

<hs> $special			{ mkL LSpecial }
<hs> @reservedid		{ mkL LReservedId }
<hs> @extreservedid		{ mkL LReservedId }
<hs> @specialid	                { mkL LVarId }
<hs> @reservedop		{ mkL LReservedOp }

--------------------
-- variables/symbols
--------------------

<hs> @varid		{ mkL LVarId }
<hs> @conid		{ mkL LConId }
<hs> @varsym		{ mkL LVarSym }
<hs> @consym		{ mkL LConSym }

<hs> (@conid \.)+ @reservedid   { mkL LQReservedId }
<hs> (@conid \.)+ @varid        { mkL LQVarId }
<hs> (@conid \.)+ @conid	{ mkL LQConId }
<hs> (@conid \.)+ @varsym	{ mkL LQVarSym }
<hs> (@conid \.)+ @consym	{ mkL LQConSym }

-----------
-- literals
-----------

<hs> @integer                   { mkL LInteger }

<hs> @decimal \. @decimal @exponent?
  | @decimal @exponent		{ mkL LFloat }

<hs> \' ($graphic # [\'\\] | " " | @escape) \'
				{ mkL LChar }

<hs> \" @string* \"	{ mkL LString }   -- "

-----------
-- unboxed
-----------

<hs> "(#" / { isEnabled FO.UnboxedTuples }  { mkL LSpecial }
<hs> "#)" / { isEnabled FO.UnboxedTuples }  { mkL LSpecial }
<hs> @integer "#" / { isEnabled FO.UnboxedValues } { mkL LInteger_ }
<hs> @stringlit "#" / { isEnabled FO.UnboxedValues } { mkL LString_ }

{
data Lexeme = L SrcLoc LexemeClass String
    deriving(Show)

data LexemeClass
  = LInteger
  | LInteger_
  | LFloat
  | LChar
  | LString
  | LString_
  | LSpecial
  | LReservedId
  | LReservedOp
  | LVarId
  | LQVarId
  | LConId
  | LQConId
  | LVarSym
  | LQVarSym
  | LConSym
  | LQConSym
  | LPragmaStart
  -- cues to post process
  | LQReservedId
  | LEOF
  deriving (Eq,Show)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p,_,_,str) len = return (L (toSrcLoc p) c (take len str))

-- override value
mkJL :: LexemeClass -> String -> AlexInput -> Int -> Alex Lexeme
mkJL c av (p,_,_,_) _ = return (L (toSrcLoc p) c av)

toSrcLoc (AlexPn _ l c) = SrcLoc {
    srcLocFileName = fileNameUnknown,
    srcLocLine = l, srcLocColumn = c }

nested_comment :: AlexInput -> Int -> Alex Lexeme
nested_comment _ _ = do
  input <- alexGetInput
  go 1 input
  where go 0 input = do alexSetInput input; alexMonadScan
	go n input = do
	  case alexGetChar input of
	    Nothing  -> err input
	    Just (c,input) -> do
	      case c of
	    	'-' -> do
		  case alexGetChar input of
		    Nothing  -> err input
		    Just ('}',input) -> go (n-1) input
		    Just (_c,_input)      -> go n input
	     	'{' -> do
		  case alexGetChar input of
		    Nothing  -> err input
		    Just ('-',input) -> go (n+1) input
		    Just (c,input)   -> go n input
	    	c -> go n input
        err input = do alexSetInput input; lexError "error in nested comment"

{-
alexGetChar' :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar' (AI loc s)
  | atEnd s   = Nothing
  | otherwise = c `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (c, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        -}

lexError :: String -> Alex a
lexError s = do
  (p,c,bs,input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++
		   (if (not (null input))
		     then " before " ++ show (head input)
		     else " at end of file"))

scanner :: Opt -> String -> Either String [Lexeme]
scanner opt str = runAlex str $ do
    alexSetUserState opt
    let loop = do tok@(L _ cl s) <- alexMonadScan;
                    if cl == LEOF
                            then return []
                            else do (tok:) `liftM` loop
    loop

alexEOF :: Alex Lexeme
alexEOF = do
    (p,_,_,_) <- alexGetInput
    return (L (toSrcLoc p) LEOF "")

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

--alexGetChar ai = alexGetByte ai
alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
--alexGetChar (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetChar (p,c,[],[]) = Nothing
alexGetChar (p,_,[],(c:s))  = let p' = alexMove p c
                              in p' `seq`  Just (c, (p', c, [], s))

type AlexUserState = Opt
alexInitUserState = options

isEnabled
    :: FO.Flag
    -> Opt       -- predicate state
    -> AlexInput  -- input stream before the token
    -> Int        -- length of the token
    -> AlexInput  -- input stream after the token
    -> Bool       -- True <=> accept the token
isEnabled fo opt _ _ _= fopts' opt fo

----------------------------------------------------------------------
-- modified from monadUserState wrapper to pass state into conditional
----------------------------------------------------------------------

type AlexAction result = AlexInput -> Int -> Alex result
type Byte = Word8
type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string
data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  us <- alexGetUserState
  case alexScanUser us inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord where
    go oc
        | oc <= 0x7f   = [oc]
        | oc <= 0x7ff  = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                         , 0x80 + oc Data.Bits..&. 0x3f
                         ]
        | oc <= 0xffff = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                         , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                         , 0x80 + oc Data.Bits..&. 0x3f
                         ]
        | otherwise    = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                         , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                         , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                         , 0x80 + oc Data.Bits..&. 0x3f
                         ]

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,bs,s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,c,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int,        -- the current startcode
        alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Alex a -> Either String a
runAlex input (Alex f)
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input,
                        alex_chr = '\n',
                        alex_bytes = [],

                        alex_ust = alexInitUserState,

                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp} ->
        Right (s, (pos,c,bs,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp} of
                  s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError message = Alex $ \s -> Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s,ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust=ss}, ())

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code input len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

token :: (AlexInput -> Int -> token) -> AlexAction token
token t input len = return (t input len)

}
