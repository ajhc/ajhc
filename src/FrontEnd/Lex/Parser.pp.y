{-# OPTIONS_GHC -w -XNoOverloadedStrings #-} {- -*- Haskell -*- -}

-- Since in general LALR parser generators produce horrible error messages, we
-- try to be as permissive as possible in this parser and then produce more
-- appropriate errors in the desugaring pass.

{
module FrontEnd.Lex.Parser (parse) where

import Control.Monad.Error
import FrontEnd.HsSyn
import FrontEnd.Lex.Lexer
import FrontEnd.SrcLoc
import Name.Name

}

%token

#sub token { my ($a,$b,$c) = @_; say  "    '$b' { L \$\$ $a \"$b\" }" }
#map { token('LSpecial',$_) } qw/{ } , ; [ ] ` ( )/
#map { token('LSpecial',$_) } qw/(# #) ∀ ∃/
#map { token('LReservedId',$_) } qw/as case class data default deriving do else hiding if import in infix infixl infixr instance let module newtype of qualified then type where/
#map { token('LReservedId',$_) } qw/kind alias prefix prefixy forall exists/
#map { token('LReservedOp',$_) } qw/.. : :: = \\\\ | <- -> @ ~ =>/

--  LSpecial { L _ LSpecial $$ }
--  LReservedId { L _ LReservedId $$ }
--  LReservedOp { L _ LReservedOp $$ }
  '_' { L $$ LVarId "_" }

  LVarId { L _ LVarId $$ }
  LQVarId { L _ LQVarId $$ }
  LConId { L _ LConId $$ }
  LQConId { L _ LQConId $$ }
  LVarSym { L _ LVarSym $$ }
  LQVarSym { L _ LQVarSym $$ }
  LConSym { L _ LConSym $$ }
  LQConSym { L _ LQConSym $$ }

  LInteger { L _ LInteger $$ }
  LFloat { L _ LFloat $$ }
  LChar { L _ LChar $$ }
  LString { L _ LString $$ }
  LEOF { L _ LEOF $$ }

%monad { Either String } { (>>=) } { return }
%name parse exp
%tokentype { Lexeme }
%%

-- some combinators for generating rules
#def maybe "m_$a : " . ($b // $a) . "  { Just \$1 }\n   |  { Nothing }\n"
#def clist "rev_cl_$a : rev_cl_$a ',' $a  { \$3:\$1 }\n   | $a { [\$1] }\n cl_$a : rev_cl_$a { reverse \$1 }\n"
#def eclist "rev_ecl_$a : rev_ecl_$a ',' $a  { \$3:\$1 }\n   | { [] }\n ecl_$a : rev_ecl_$a { reverse \$1 }\n"
#def slist "rev_sl_$a : rev_sl_$a $a  { \$2:\$1 }\n   | $a { [\$1] }\n sl_$a : rev_sl_$a { reverse \$1 }\n"

lit :: { HsLiteral }
    : LInteger  { HsInt $ read $1 }
    | LChar     { HsChar $ read $1 }
    | LString   { HsString $ read $1 }

expp :: { HsExp }
    : '(' ecl_exp ')' { espan $1 $3 $ mtuple $2 }
    | '(#' ecl_exp '#)' { espan $1 $3 $ HsUnboxedTuple $2 }
    | '[' ecl_exp ']' { espan $1 $3 $ HsList $2 }
    | '[' exp '..' ']' { espan $1 $4 $ HsEnumFrom $2 }
    | '[' exp ',' exp '..' ']' { espan $1 $5 $ HsEnumFromThen $2 $4 }
    | '[' exp '..' exp ']' { espan $1 $5 $ HsEnumFromTo $2 $4 }
    | '[' exp ',' exp '..' exp ']' { espan $1 $7 $ HsEnumFromThenTo $2 $4 $6 }
    | '_' { HsWildCard $1 }
    | var { HsVar $1 }
    | con { HsCon $1 }
    | lit { HsLit $1 }

list :: { HsExp }   
--    : exp  { HsList [$1] }
    : cl_exp               { HsList $1 }
    | exp '..'             { HsEnumFrom $1 }
    | exp ',' exp '..'     { HsEnumFromThen $1 $3 }
    | exp '..' exp         { HsEnumFromTo $1 $3 }
    | exp ',' exp '..' exp { HsEnumFromThenTo $1 $3 $5 }
    |                      { HsList [] }

pat :: { HsExp }
    : expp { $1 }

var :: { Name }
    : LVarId  { (toName UnknownType $1) }
    | LQVarId  {(toName UnknownType $1) }
con :: { Name }
    : LConId  { (toName UnknownType $1) }
    | LQConId { (toName UnknownType $1) }

exp :: { HsExp }
    : 'if' exp 'then' exp 'else' exp { HsIf (espan $1 $3 $ $2) (espan $3 $5 $4) $6 }
--    | '\' rsl_pat '->' exp { HsLambda $1 $2 $4 }
    |  sl_expp { HsWords $1 }
    | expp { $1 }


#maybe 'comma', "','"
#maybe 'semi', "';'"

#clist 'exp'
#eclist 'exp'
#slist 'expp'
#slist 'pat'

{
happyError = fail "parse error"

mtuple [x] = HsParen x
mtuple xs = HsTuple xs

eloc p e =  HsLocatedExp (Located (srcSpan p) e)
espan p1 p2 e =  HsLocatedExp (Located (SrcSpan p1 p2) e)
}
