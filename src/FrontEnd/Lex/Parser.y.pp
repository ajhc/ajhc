{-# OPTIONS_GHC -w -XNoOverloadedStrings #-} {- -*- Haskell -*- -}

-- Since in general LALR parser generators produce horrible error messages, we
-- try to be as permissive as possible in this parser and then produce more
-- appropriate errors in the desugaring pass.

{
module FrontEnd.Lex.Parser (parse,parseDecls,parseStmt,parseModule) where

import Control.Monad.Error
import FrontEnd.HsSyn
import FrontEnd.Lex.Lexer
import FrontEnd.Lex.ParseMonad
import FrontEnd.Lex.Post
import FrontEnd.SrcLoc
import FrontEnd.Warning
import Name.Name
import Name.Names

}

%token

#sub token { my ($a,$b,$c) = @_; say  "    '$b' { L \$\$ $a \"$b\" }" }
#map { token('LSpecial',$_) } qw/{ } , ; [ ] ` ( )/
#map { token('LSpecial',$_) } qw/(# #) ∀ ∃/
#map { token('LReservedId',$_) } qw/as case class data default deriving do else hiding if import in infix infixl infixr instance let module newtype of qualified then type where/
#map { token('LReservedId',$_) } qw/kind alias prefixx prefixy forall exists family closed/
#map { token('LReservedOp',$_) } qw/.. : :: = \\\\ | <- -> @ ~ =>/

 '_' { L $$ LVarId "_" }

  LVarId   { L _ LVarId $$ }
  LQVarId  { L _ LQVarId $$ }
  LConId   { L _ LConId $$ }
  LQConId  { L _ LQConId $$ }
  LVarSym  { L _ LVarSym $$ }
  LQVarSym { L _ LQVarSym $$ }
  LConSym  { L _ LConSym $$ }
  LQConSym { L _ LQConSym $$ }

  LInteger { L _ LInteger $$ }
  LFloat   { L _ LFloat $$ }
  LChar    { L _ LChar $$ }
  LString  { L _ LString $$ }
  LEOF     { L _ LEOF $$ }

%monad { P } { (>>=) } { return }
%name parse exp
%name parseDecls decls
%name parseStmt stmt
%name parseModule module
%tokentype { Lexeme }
%%

-- some combinators for generating rules
#def 'qw' maybe "m_$a : " . ($b // $a) . "  { Just \$1 }\n   |  { Nothing }\n"
#def 'qw' wlist "rev_wl_$a : rev_wl_$a $a  { \$2:\$1 }\n   | $a { [\$1] }\n wl_$a : rev_wl_$a { reverse \$1 }\n"
#def 'qw' ewlist "rev_ewl_$a : rev_ewl_$a $a  { \$2:\$1 }\n   | { [] }\n ewl_$a : rev_ewl_$a { reverse \$1 }\n"

#[def 'qw' clist qq[
rev_cl_$a
    : rev_cl_$a ',' $a  { \$3:\$1 }
    | $a { [\$1] }

cl_$a : rev_cl_$a { reverse \$1 }
ecl_$a
    : cl_$a { \$1 }
    |       { [] }
cl2_$a
    : $a ',' cl_$a { \$1:\$3 }
]
#]

#[def 'qw' blist qq[
rev_bl_$a
    : rev_bl_$a '|' $a  { \$3:\$1 }
    | $a { [\$1] }

bl_$a : rev_bl_$a { reverse \$1 }
ebl_$a
    : bl_$a { \$1 }
    |       { [] }
]
#]

module :: { HsModule }
    : '{' impdecls decls '}'  { hsModule {
        hsModuleName    = toModule "Main",
        hsModuleExports = Just [HsEVar (toName Val "main")],
        hsModuleSrcLoc  = $1,
        hsModuleImports = $2,
        hsModuleDecls   = fixupHsDecls $3
    } }
    | 'module' modid m_exports 'where' '{' impdecls decls '}' { hsModule {
        hsModuleName    = $2,
        hsModuleExports = $3,
        hsModuleSrcLoc  = $1,
        hsModuleImports = $6,
        hsModuleDecls   = fixupHsDecls $7
        } }

#maybe exports

pat :: { HsPat }
    : aexp {% checkPattern $1  }

decl :: { HsDecl }
    : exp0 srcloc rhs optwhere  {% checkValDef $2 $1 $3 $4 }
    | cl_fixvar '::' qualtype { HsTypeSig $2 $1 $3 }
    | 'type' con ewl_atype '=' type
                      { HsTypeDecl {
                        hsDeclSrcLoc = $1,
                        hsDeclName = $2,
                        hsDeclTArgs = $3,
                        hsDeclType = $5 } }
    | assoc INT cl_varconop  { HsInfixDecl (fst $1) (snd $1) $2 $3 }
    -- | 'data' mCTYPE ctype '::' kind srcloc deriving
    --       {% checkDataHeader $3 `thenP` \(cs,c,t) ->
    --          returnP hsDataDecl { hsDeclSrcLoc = $6, hsDeclContext = cs, hsDeclName = c, hsDeclArgs = t, hsDeclDerives = $7, hsDeclHasKind = Just $5, hsDeclCTYPE = $2 } }
    | 'data' qualtype mconstrs deriving {% do
        (cs,c,t) <- checkDataHeader $2
        return hsDataDecl {
            hsDeclSrcLoc = $1, hsDeclContext = cs,
            hsDeclName = c, hsDeclArgs = t, hsDeclDerives = $4,
            hsDeclCons = $3 } }
    | 'newtype' qualtype '=' constr deriving {% do
        (cs,c,t) <- checkDataHeader $2
        return hsNewTypeDecl {
            hsDeclSrcLoc = $1, hsDeclContext = cs,
            hsDeclName = c, hsDeclArgs = t, hsDeclDerives = $5,
            hsDeclCons = [$4] } }
    | 'instance' classhead optwhere { HsInstDecl $1 $2 $3 }
    | 'class' classhead optwhere { HsClassDecl $1 $2 $3 }

classhead :: { HsClassHead }
    : qualtype {% qualTypeToClassHead $1 }


mconstrs :: { [HsConDecl] }
    : '=' bl_constr { $2 }
--    | '::' kind
    |              { [] }

deriving :: { [Name] }
      : {- empty -}               { [] }
      | 'deriving' con            { [$2] }
      | 'deriving' '(' ')'        { [] }
      | 'deriving' '(' cl_con ')' { $3 }

constr :: { HsConDecl }
      : srcloc mexists scontype   { HsConDecl {
            hsConDeclSrcLoc = $1,
            hsConDeclName = (fst $3),
            hsConDeclConArg = (snd $3),
            hsConDeclExists = $2 } }
      | srcloc mexists gcon '{' ecl_fielddecl '}' { HsRecDecl {
            hsConDeclSrcLoc = $1,
            hsConDeclName = $3,
            hsConDeclRecArg = $5,
            hsConDeclExists = $2 } }

fielddecl :: { ([HsName],HsBangType) }
      : cl_var '::' type          { ($1, HsUnBangedTy $3) }

      -- | srcloc mexists sbtype conop sbtype    { HsConDecl {
      --       hsConDeclSrcLoc = $1,
      --       hsConDeclName = $4,
      --       hsConDeclConArg = [$3,$5],
      --       hsConDeclExists = $2 } }
      -- | srcloc mexists con '{' fielddecls '}' { HsRecDecl {
      --       hsConDeclSrcLoc = $1,
      --       hsConDeclName = $3,
      --       hsConDeclRecArg = (reverse $5),
      --       hsConDeclExists = $2 } }
      -- | srcloc mexists con '{' '}' { HsRecDecl {
      --       hsConDeclSrcLoc = $1,
      --       hsConDeclName = $3,
      --       hsConDeclRecArg = [],
      --       hsConDeclExists = $2 } }
mexists : { [] }

scontype :: { (HsName, [HsBangType]) }
    : wl_batype {% checkSconType $1  }

batype :: { Either Name HsType }
    : varconop    { Left $1 }
    | atype       { Right $1 }

rhs :: { HsRhs }
    : '=' exp   { HsUnGuardedRhs $2 }
    | wl_gdrh   { HsGuardedRhss $1 }

gdrh :: { HsGuardedRhs }
      : '|' exp '=' exp        { HsGuardedRhs $1 $2 $4 }

rhs_case :: { HsRhs }
    : '->' exp   { HsUnGuardedRhs $2 }
    | wl_gdrh_case   { HsGuardedRhss $1 }

gdrh_case :: { HsGuardedRhs }
      : '|' exp '->' exp        { HsGuardedRhs $1 $2 $4 }

assoc :: { (SrcLoc,HsAssoc) }
    : 'infix'  { ($1,HsAssocNone) }
    | 'infixl'  { ($1,HsAssocLeft) }
    | 'infixr'  { ($1,HsAssocRight) }
    | 'prefixx'  { ($1,HsAssocPrefix) }
    | 'prefixy'  { ($1,HsAssocPrefixy) }

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
      : exports                               { Just (False,$1) }
      | 'hiding' exports                      { Just (True,$2) }
      | {- empty -}                           { Nothing }

exports :: { [HsExportSpec] }
    : '(' ecl_export ')'     { $2 }

export :: { HsExportSpec }
      :  var                          { HsEVar $1 }
      |  con                          { HsEAbs $1 }
      |  con '(' '..' ')'             { HsEThingAll $1 }
      |  con '(' ecl_varcon ')'       { HsEThingWith $1 $3 }
      |  'module' modid               { HsEModuleContents $2 }

type :: { HsType }
      : btype '->' type               { HsTyFun $1 $3 }
      | btype                         { $1 }
--      | 'forall' tbinds '.' qualtype  { HsTyForall { hsTypeVars = reverse $2, hsTypeType = $4 } }
--      | 'exists' tbinds '.' qualtype  { HsTyExists { hsTypeVars = reverse $2, hsTypeType = $4 } }

btype :: { HsType }
      : btype atype                   { HsTyApp $1 $2 }
      | atype                         { $1 }

atype :: { HsType }
      : con                    { HsTyCon $1 }
      | var                    { HsTyVar $1 }
      | '(' ')'                { HsTyCon tc_Unit }
      | '[' ']'                { HsTyCon tc_List }
      | '(' cl2_type ')'       { HsTyTuple $2 }
      | '(#' ecl_type '#)'     { HsTyUnboxedTuple $2 }
      | '[' type ']'           { HsTyApp (HsTyCon (toName UnknownType "[]")) $2 }
      | '(' type ')'           { $2 }
      | '(' type '=' type ')'  { HsTyEq $2 $4 }

qualtype :: { HsQualType }
      : type '=>' type             {% withSrcLoc $2 $ checkContext $1 >>= return . flip HsQualType $3 }
      | type                       { HsQualType [] $1 }

exp :: { HsExp }
    : exp0 '::' qualtype { HsExpTypeSig $2 $1 $3 }
    | exp0       { $1 }

exp0  :: { HsExp }
    : exp1 { $1 }
    | aexp exp0 { $1 `cat` $2 }

exp1 :: { HsExp }
    : 'if' exp 'then' exp 'else' exp { HsIf (espan $1 $3 $ $2) (espan $3 $5 $4) (eloc $5 $6) }
    | '\\' wl_pat '->' exp { HsLambda $1 $2 $4 }
    | 'let' '{' decls '}' 'in' exp { HsLet (fixupHsDecls $3) $6 }
    | 'case' exp 'of' '{' alts '}'  { espan $1 $6 $ HsCase (espan $1 $3 $2) $5 }
    | aexp  { $1 }

stmt :: { HsStmt }
      : pat '<-' exp      { HsGenerator $2 $1 $3 }
      | exp               { HsQualifier $1 }
      | 'let' '{' decls '}'    { HsLetStmt  (fixupHsDecls $3) }

alt :: { HsAlt }
    : srcloc pat rhs_case optwhere { HsAlt $1 $2 $3 $4 }
 --   : pat '->' exp { HsAlt $2 $1 (HsUnGuardedRhs $3) [] }

aexp :: { HsExp }
    : '(' ecl_exp ')'   { espan $1 $3 $ mtuple $2 }
    | '(#' ecl_exp '#)' { espan $1 $3 $ HsUnboxedTuple $2 }
--    | '[' ']'           { espan $1 $2 $ HsList [] }
    | '[' list ']'      { espan $1 $3 $2 }
    | '_'               { HsWildCard $1 }
    | var               { HsVar $1 }
    | con               { HsCon $1 }
    | varop             { HsVar $1 }
    | conop             { HsCon $1 }
    | '`' var '`'       { HsBackTick (HsVar $2) }
    | '`' con '`'       { HsBackTick (HsCon $2) }
    | lit               { HsLit $1 }
    -- atomic after layout processing
    | 'do' '{' stmts  '}'       { HsDo $3 }

commas :: { Int }
      : commas ','                    { $1 + 1 }
      | ','                           { 1 }

fbind :: { (Name,Maybe HsExp) }
    : uqvar '=' exp  { ($1,Just (eloc $2 $3)) }
    | uqvar { ($1,Nothing) }

optwhere :: { [HsDecl] }
      : 'where' '{' decls  '}'                { fixupHsDecls $3 }
      | {- empty -}                           { [] }

list :: { HsExp }
    : ecl_exp                 { HsList $1 }
    | exp '|' cl_stmt         { HsListComp $1 $3 }
    | cl_exp '..'             {% case $1 of
        [x]   -> return $ HsEnumFrom x
        [x,y] -> return $ HsEnumFromThen x y
        _ -> fail "parse error in list comprehension" }
    | cl_exp '..' exp         {% case $1 of
        [x]   -> return $ HsEnumFromTo x $3
        [x,y] -> return $ HsEnumFromThenTo x y $3
        _ -> fail "parse error in list comprehension" }
--    | exp '..'                { HsEnumFrom $1 }
--    | exp '..' exp            { HsEnumFromTo $1 $3 }
--    | exp0 ',' exp0 '..' exp0 { HsEnumFromThenTo $1 $3 $5 }

lit :: { HsLiteral }
    : LInteger  { HsInt $ read $1 }
    | LChar     { HsChar $ read $1 }
    | LString   { HsString $ read $1 }

INT :: { Int }
    : LInteger { read $1 }

#map { clist $_ } qw/exp stmt type var varcon varconop export con fielddecl fixvar/
#blist constr
#wlist aexp
#wlist gdrh
#wlist batype
#wlist gdrh_case
#ewlist aexp
#ewlist atype
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

fixvar :: { Name }
    : '(' varop ')' { $2 }
    | var   { $1 }

var :: { Name }
    : uqvar { $1 }
    | LQVarId  {(toName UnknownType $1) }

uqvar :: { Name }
    : LVarId  { (toName UnknownType $1) }
    | 'as'                  { u_as }
    | 'alias'               { u_alias }
    | 'kind'                { u_kind }
    | 'closed'              { u_closed }
    | 'family'              { u_family }
    | 'qualified'           { u_qualified }
    | 'hiding'              { u_hiding }
    | 'forall'              { u_forall }
    | 'exists'              { u_exists }

gcon :: { Name }
    : '(' commas ')'   { tuple_con_name $2 }
    | con              { $1 }
    | '(' conop  ')'   { $2 }

con :: { Name }
    : LConId  { (toName UnknownType $1) }
    | LQConId { (toName UnknownType $1) }

varcon
    : var { $1 }
    | con { $1 }
    | '(' varop ')' { $2 }
    | '(' conop ')' { $2 }

varconop
    : varop { $1 }
    | conop { $1 }
    | '`' var '`' { $2 }
    | '`' con '`' { $2 }

conop :: { Name }
    : LConSym  { (toName UnknownType $1) }
    | LQConSym { (toName UnknownType $1) }
    | ':'      { (toName UnknownType ":") }

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

srcloc :: { SrcLoc } :       {%^ \ (L sl _ _) -> return sl }

{

happyError [] = do
    addWarn ParseError "parse error at EOF"
    parseNothing
happyError (L sl _ t:_) = do
    warn sl ParseError $ "parse error at " ++ show t
    parseNothing

mtuple [x] = HsParen x
mtuple xs = HsTuple xs

hsWords [] = error "hsWords []"
hsWords [x] = x
hsWords xs = HsWords xs

x `cat` HsWords ws = HsWords (x:ws)
x `cat` y = HsWords [x,y]

eloc p e =  HsLocatedExp (Located (srcSpan p) e)
espan p1 p2 e =  HsLocatedExp (Located (SrcSpan p1 p2) e)

tuple_con_name i = toName DataConstructor (toModule "Jhc.Prim.Prim","("++replicate i ','++")")

}
