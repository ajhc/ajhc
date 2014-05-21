{
{-# OPTIONS_GHC -w -XNoOverloadedStrings #-} {- -*- Haskell -*- -}
module FrontEnd.Lex.Lexer (scanner,Lexeme(..),LexemeClass(..),AlexPosn(..)) where

import Control.Monad
import Name.Name
import FrontEnd.Lexer(Token(..))
import FrontEnd.SrcLoc
}

%wrapper "monad"

$whitechar = [ \t\n\r\f\v]
$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

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

@reservedid =
	as|case|class|data|default|deriving|do|else|hiding|if|
	import|in|infix|infixl|infixr|instance|let|module|newtype|
	of|qualified|then|type|where

@reservedop =
	".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"

@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*

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
@ws      = $white+

haskell :-

<0>  ^"#!" .*              { begin hs }
<0>  ""                    { begin hs }
<hs> $white+		   ;
<hs> "--"\-*[^$symbol].*   ;
<hs> "--"\-*$	           ;

-- Handle CPP style line pragmas
-- <hs> ^"#line " @ws                { mkJL LPragmaStart "LINE" `andBegin` line_pragma }
-- <hs> ^"# " / @decimal          { mkJL LPragmaStart "LINE" `andBegin` line_pragma }
-- <line_pragma> $white*$          { mkJL LSpecial "#-}" `andBegin` 0 }

"{-#"                           { mkL LSpecial }
"#-}"                           { mkL LSpecial }

"{-"				{ nested_comment }
<hs> $special			{ mkL LSpecial }

<hs> @reservedid			{ mkL LReservedId }
<hs> (@conid \.)+ @varid		{ mkL LQVarId }
<hs> (@conid \.)+ @conid		{ mkL LQConId }
<hs,line_pragma> @varid		{ mkL LVarId }
<hs,line_pragma> @conid		{ mkL LConId }

<hs> @reservedop			{ mkL LReservedOp }
<hs> (@conid \.)+ @varsym	{ mkL LVarSym }
<hs> (@conid \.)+ @consym	{ mkL LConSym }
<hs> @varsym			{ mkL LVarSym }
<hs> @consym			{ mkL LConSym }

<hs,line_pragma> @decimal
  | 0[oO] @octal
  | 0[xX] @hexadecimal		{ mkL LInteger }

<hs> @decimal \. @decimal @exponent?
  | @decimal @exponent		{ mkL LFloat }

<hs> \' ($graphic # [\'\\] | " " | @escape) \'
				{ mkL LChar }

<hs,line_pragma> \" @string* \"	{ mkL LString }   -- "

{
data Lexeme = L SrcLoc LexemeClass String
    deriving(Show)

data LexemeClass
  = LInteger
  | LFloat
  | LChar
  | LString
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

{-
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
-}

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

scanner :: String -> Either String [Lexeme]
scanner str = runAlex str $ do
  let loop = do tok@(L _ cl s) <- alexMonadScan;
		  if cl == LEOF
			then return []
			else do (tok:) `liftM` loop
  loop

alexEOF :: Alex Lexeme
alexEOF = do
    (p,_,_,_) <- alexGetInput
    return (L (toSrcLoc p) LEOF "")

--scanner :: String -> Either String [Lexeme]
--scanner = Right . filter (not . isLWhiteSpace . lexemeClass) . alexScanTokens
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

main = do
  s <- getContents
  --mapM_ print (alexScanTokens s)
  print (scanner s)

--alexGetChar ai = alexGetByte ai
alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
--alexGetChar (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetChar (p,c,[],[]) = Nothing
alexGetChar (p,_,[],(c:s))  = let p' = alexMove p c
                              in p' `seq`  Just (c, (p', c, [], s))

}
