if exists("b:current_syntax")
  finish
endif


syn match hsType "\<int\>\|\<char\>\|\<bool\>\|\<float\>\|\<double\>\|\<void\>"
syn match hsType "\<Hs[A-Z][a-zA-Z]*\>"
syn match hsType "\<[usfc]\?bits[0-9]\+\>"
syn match hsType "\<[usfc]\?bits<[A-Za-z_]\+>"
syn match hsType "\<[a-z]\+_t\>"

syn match grinFuncname "\<[fb][^i][^" ]*\>"
syn match grinFuncname "\<@main\>"
syn match grinWHNF  "\<[CP][^" ]*\>"
syn match grinSusp  "\<[FB][^" ]*\>"
syn match grinVar "\<[pnlcitv]\+-\?[0-9]\+\>"

syn match hsDelimiter  "(\|)\|\[\|\]\|,\|;\|_\|{\|}"

syn match   hsSpecialChar	contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|^[A-Z^_\[\\\]]\)"
syn match   hsSpecialChar	contained "\\\(NUL\|SOH\|STX\|ETX\|EOT\|ENQ\|ACK\|BEL\|BS\|HT\|LF\|VT\|FF\|CR\|SO\|SI\|DLE\|DC1\|DC2\|DC3\|DC4\|NAK\|SYN\|ETB\|CAN\|EM\|SUB\|ESC\|FS\|GS\|RS\|US\|SP\|DEL\)"
syn match   hsSpecialCharError	contained "\\&\|'''\+"
syn region  hsString		start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=hsSpecialChar
syn match   hsCharacter		"[^a-zA-Z0-9_']'\([^\\]\|\\[^']\+\|\\'\)'"lc=1 contains=hsSpecialChar,hsSpecialCharError
syn match   hsCharacter		"^'\([^\\]\|\\[^']\+\|\\'\)'" contains=hsSpecialChar,hsSpecialCharError
syn match   hsNumber		"\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>"
syn match   hsFloat		"\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"

" Comments
syn match   hsLineComment      "--.*"
syn region  hsBlockComment     start="{-"  end="-}" contains=hsBlockComment
syn region  hsPragma           start="{-#" end="#-}"

syn match hsOperator "->\|<-\|=\|&\|:=\|+\|-\|==\|!=\|>\|>=\|<\|<=\|*\|/"
"syn match hsVarSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[-!#$%&\*\+/<=>\?@\\^|~.][-!#$%&\*\+/<=>\?@\\^|~:.]*"
"syn match hsConSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=:[-!#$%&\*\+./<=>\?@\\^|~:]*"
"syn match hsVarSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[a-z][a-zA-Z0-9_']*`"
"syn match hsConSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[A-Z][a-zA-Z0-9_']*`"

syn keyword grinControl do case return of error let
syn keyword grinMemory store fetch alloc update promote demote
syn keyword grinEvalKeyword eval apply
syn match grinEvalKeyword "\<@apply_\?[0-9]*\>"

let b:current_syntax = "grin"

  hi link hsBlockComment		  hsComment
  hi link hsLineComment			  hsComment
  hi link hsComment			  Comment
  hi link hsPragma			  SpecialComment
  hi link grinControl			  Conditional
  hi link grinEvalKeyword		  Conditional
  hi link hsString			  String
  hi link hsSpecialCharError		  Error
  hi link hsSpecialChar			  SpecialChar
  hi link grinMemory			  Statement
  hi link hsCharacter			  Character
  hi link hsNumber			  Number
  hi link hsFloat			  Float
  hi link hsDelimiter			  Delimiter
  hi link hsType			  Type
  hi link hsVarSym			  hsOperator
  hi link hsConSym			  hsOperator
  hi link hsOperator			  Operator
  hi link grinFuncname                    Structure


set iskeyword+=.,$

