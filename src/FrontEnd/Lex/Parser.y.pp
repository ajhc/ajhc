{-# OPTIONS_GHC -w -XNoOverloadedStrings #-} {- -*- Haskell -*- -}

-- Since in general LALR parser generators produce horrible error messages, we
-- try to be as permissive as possible in this parser and then produce more
-- appropriate errors in the desugaring pass.

{
module FrontEnd.Lex.Parser (parse,parseDecls,parseStmt,parseModule) where

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
%name parseDecls decls
%name parseStmt stmt
%name parseModule module
%tokentype { Lexeme }
%%

-- some combinators for generating rules
#def 'qw' maybe "m_$a : " . ($b // $a) . "  { Just \$1 }\n   |  { Nothing }\n"
#def 'qw' clist "rev_cl_$a : rev_cl_$a ',' $a  { \$3:\$1 }\n   | $a { [\$1] }\n cl_$a : rev_cl_$a { reverse \$1 }\n"
#def 'qw' slist "rev_sl_$a : rev_sl_$a semis $a  { \$3:\$1 }\n   | $a { [\$1] }\n sl_$a : rev_sl_$a { reverse \$1 }\n"
#def 'qw' eslist "rev_esl_$a : rev_esl_$a semis $a  { \$3:\$1 }\n   | { [] }\n esl_$a : rev_esl_$a { reverse \$1 }\n"
#def 'qw' eclist "rev_ecl_$a : rev_ecl_$a ',' $a  { \$3:\$1 }\n   | { [] }\n ecl_$a : rev_ecl_$a { reverse \$1 }\n"
#def 'qw' wlist "rev_wl_$a : rev_wl_$a $a  { \$2:\$1 }\n   | $a { [\$1] }\n wl_$a : rev_wl_$a { reverse \$1 }\n"
#def 'qw' ewlist "rev_ewl_$a : rev_ewl_$a $a  { \$2:\$1 }\n   | { [] }\n ewl_$a : rev_ewl_$a { reverse \$1 }\n"

--stmt { HsStmt }
--    : pat '<-'

module :: { [HsDecl] }
--    : '{' impdecls '}'  { [] }
--    | 'module' con 'where' '{' impdecls '}' { [] }
    : '{' impdecls decls '}'  { $3 }
    | 'module' con 'where' '{' impdecls decls '}' { $6 }
--    | '{' decls '}'  { $2 }
--    | 'module' con 'where' '{' decls '}' { $5 }

pat :: { HsPat }
    : aexp { HsPatExp $1 }
pats :: { HsPat }
    : wl_aexp { HsPatExp $ hsWords $1 }

decl :: { HsDecl }
    : pats '=' exp   { HsPatBind { hsDeclSrcLoc = srcLoc $2,
                       hsDeclPat = $1, hsDeclRhs = (HsUnGuardedRhs $3),
                       hsDeclDecls = [] } }
    | cl_var '::' qualtype { HsTypeSig $2 $1 $3 }

impdecl :: { HsImportDecl }
    : 'import' optqualified modid maybeas maybeimpspec { HsImportDecl $1 $3 $2 $4 $5 }

modid :: { Module }
    : con { toModule (show $1) }

optqualified :: { Bool }
      : 'qualified'                           { True  }
      | {- empty -}                           { False }

maybeas :: { Maybe Module }
      : 'as' modid           { Just $2 }
      | {- empty -}          { Nothing }

maybeimpspec :: { Maybe (Bool, [HsExportSpec]) }
--      : impspec                               { Just $1 }
      : {- empty -}                           { Nothing }

qualtype :: { HsQualType }
    : 'type' { error "qualtype" }

exp :: { HsExp }
    : exp0 '::' qualtype { HsExpTypeSig $2 $1 $3 }
    | exp0       { $1 }

exp0  :: { HsExp }
    : exp1 { $1 }
    | aexp exp0 { $1 `cat` $2 }

exp1 :: { HsExp }
    : 'if' exp 'then' exp 'else' exp { HsIf (espan $1 $3 $ $2) (espan $3 $5 $4) (eloc $5 $6) }
    | '\\' wl_pat '->' exp { HsLambda $1 $2 $4 }
    | 'let' '{' decls '}' 'in' exp { HsLet $3 $6 }
    | 'do' '{' stmts  '}'       { HsDo $3 }
    | 'case' exp 'of' '{' alts '}'  { espan $1 $6 $ HsCase (espan $1 $3 $2) $5 }
    | aexp  { $1 }

stmt :: { HsStmt }
      : pat '<-' exp      { HsGenerator $2 $1 $3 }
      | exp               { HsQualifier $1 }
      | 'let' '{' decls '}'    { HsLetStmt  $3 }

alt :: { HsAlt }
    : pats '->' exp { HsAlt $2 $1 (HsUnGuardedRhs $3) [] }

aexp :: { HsExp }
    : '(' ecl_exp ')' { espan $1 $3 $ mtuple $2 }
    | '(#' ecl_exp '#)' { espan $1 $3 $ HsUnboxedTuple $2 }
    | '[' ']' { espan $1 $2 $ HsList [] }
    | '[' list ']' { espan $1 $3 $2 }
    | '_' { HsWildCard $1 }
    | var { HsVar $1 }
    | con { HsCon $1 }
    | varop { HsVar $1 }
    | conop { HsCon $1 }
    | '`' var '`' { espan $1 $3 $ HsBackTick (HsVar $2) }
    | '`' con '`' { espan $1 $3 $ HsBackTick (HsCon $2) }
    | lit { HsLit $1 }

fbind :: { (Name,Maybe HsExp) }
    : uqvar '=' exp  { ($1,Just (eloc $2 $3)) }
    | uqvar { ($1,Nothing) }

list :: { HsExp }
    : cl_exp               { HsList $1 }
    | exp '..'             { HsEnumFrom $1 }
    | exp ',' exp '..'     { HsEnumFromThen $1 $3 }
    | exp '..' exp         { HsEnumFromTo $1 $3 }
    | exp ',' exp '..' exp { HsEnumFromThenTo $1 $3 $5 }

lit :: { HsLiteral }
    : LInteger  { HsInt $ read $1 }
    | LChar     { HsChar $ read $1 }
    | LString   { HsString $ read $1 }

#clist exp
#clist var
#eclist exp
#wlist aexp
#ewlist aexp
#wlist pat

#[def oslist   qq[
${a}s : rev_$a { reverse \$1 }
rev_$a
    : rev_$a ';' $a { \$3 : \$1 }
    | rev_$a ';'    { \$1 }
    | $a            { [\$1] }
    | {- empty -}   { [] }
]
#]

#oslist 'decl'
#oslist 'impdecl'
#oslist 'alt'
#oslist 'stmt'

var :: { Name }
    : uqvar { $1 }
    | LQVarId  {(toName UnknownType $1) }

uqvar :: { Name }
    : LVarId  { (toName UnknownType $1) }

con :: { Name }
    : LConId  { (toName UnknownType $1) }
    | LQConId { (toName UnknownType $1) }

conop :: { Name }
    : LConSym  { (toName UnknownType $1) }
    | LQConSym { (toName UnknownType $1) }

varop :: { Name }
    : LVarSym  { (toName UnknownType $1) }
    | LQVarSym { (toName UnknownType $1) }

-- punctuation.

#[def optpunc   qq[
opt$a :: { () }
      : $b            { () }
      | {- empty -}   { () }
${a}s :: { () }
       : opt${a}s $b  { () }
opt${a}s :: { () }
       : ${a}s        { () }
       | {- empty -}  { () } ]
#]

#optpunc 'semi',"';'"

srcloc :: { SrcLoc } :       { bogusASrcLoc }

{
happyError [] = Left "parse error at EOF"
happyError (t:_) = Left $ "parse error at " ++ show t

mtuple [x] = HsParen x
mtuple xs = HsTuple xs

hsWords [] = error "hsWords []"
hsWords [x] = x
hsWords xs = HsWords xs

x `cat` HsWords ws = HsWords (x:ws)
x `cat` y = HsWords [x,y]

eloc p e =  HsLocatedExp (Located (srcSpan p) e)
espan p1 p2 e =  HsLocatedExp (Located (SrcSpan p1 p2) e)
}
