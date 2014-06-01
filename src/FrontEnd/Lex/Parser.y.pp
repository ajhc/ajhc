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
import Info.Properties
import Name.Names
import qualified Data.Map as Map

}

%token

#def token "    '$b' { L \$\$ $a \"$b\" }\n"
#map { token('LSpecial',$_) } qw/{ } , ; [ ] ` ( )/
#map { token('LSpecial',$_) } qw/(# #) #-}/
#map { token('LReservedId',$_) } qw/case class data default deriving do else if import in infix infixl infixr instance let module newtype of then type where _/
--#map { token('LReservedId',$_) } qw/as hiding qualified/
#map { token('LReservedId',$_) } qw/kind alias prefixx prefixy forall exists family closed foreign/
-- #map { token('LReservedOp',$_) } qw/.. : :: = \\\\ | <- -> @ ~ =>/
#map { token('LReservedOp',$_) } qw/.. :: = \\\\ | <- -> =>/
#def pragma qq[    '$a' { L _ LPragmaStart \$\$@"$a" }\n]

#map {pragma($_)} qw/NOINLINE CATALYST SPECIALIZE MULTISPECIALIZE SUPERSPECIALIZE RULE NOETA SUPERINLINE CTYPE INLINE SRCLOC_ANNOTATE/
 '.' { L $$ LVarSym "." }
 'as' { L $$ LVarId "as" }
 'hiding' { L $$ LVarId "hiding" }
 'qualified' { L $$ LVarId "qualified" }

 cons_id  { L $$ LVarId ":" }
 cons_sym { L $$ LVarSym ":" }

  LVarId   { L _ LVarId $$ }
  LQVarId  { L _ LQVarId $$ }
  LConId   { L _ LConId $$ }
  LQConId  { L _ LQConId $$ }
  LVarSym  { L _ LVarSym $$ }
  LQVarSym { L _ LQVarSym $$ }
  LConSym  { L _ LConSym $$ }
  LQConSym { L _ LQConSym $$ }

  LInteger  { L _ LInteger $$ }
  LInteger_ { L _ LInteger_ $$ }
  LFloat    { L _ LFloat $$ }
  LFloat_    { L _ LFloat_ $$ }
  LChar     { L _ LChar $$ }
  LChar_     { L _ LChar_ $$ }
  LString   { L _ LString $$ }
  LString_  { L _ LString_ $$ }

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
        hsModuleName    = mod_Main,
        hsModuleExports = Just [HsEVar vu_main],
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

pats :: { [HsPat] }
    : srcloc exp0 {% withSrcLoc $1 (checkPatterns $2) }

epat :: { HsPat }
    : exp {% checkPattern $1 }

CTYPE :: { String } :  'CTYPE' STRING '#-}'  { $2 }
#maybe CTYPE

decl :: { HsDecl }
    : exp0 srcloc rhs optwhere  {% checkValDef $2 $1 $3 $4 }
    | cl_var '::' qualtype { HsTypeSig $2 $1 $3 }
    | 'type' con ewl_atype '=' type
                      { HsTypeDecl {
                        hsDeclSrcLoc = $1,
                        hsDeclName = nameTyLevel_u (const typeLevel) $2,
                        hsDeclTArgs = $3,
                        hsDeclType = $5 } }
    | assoc INT cl_varconop  { HsInfixDecl (fst $1) (snd $1) $2 $3 }
    | 'data' m_CTYPE qualtype mconstrs deriving {% withSrcLoc $1 $ do
        (cs,c,t) <- checkDataHeader $3
        return hsDataDecl {
            hsDeclSrcLoc = $1, hsDeclContext = cs,
            hsDeclName = c, hsDeclArgs = t, hsDeclDerives = $5,
            hsDeclCons = fst $4, hsDeclHasKind = snd $4, hsDeclCTYPE = $2 } }
    | 'newtype' m_CTYPE qualtype '=' constr deriving {% withSrcLoc $1 $ do
        (cs,c,t) <- checkDataHeader $3
        return hsNewTypeDecl {
            hsDeclSrcLoc = $1, hsDeclContext = cs,
            hsDeclName = c, hsDeclArgs = t, hsDeclDerives = $6,
            hsDeclCons = [$5], hsDeclCTYPE = $2 } }
    | 'instance' classhead optwhere { HsInstDecl $1 $2 $3 }
    | 'class' classhead optwhere { HsClassDecl $1 $2 $3 }
    | 'foreign' 'import' ewl_var mstring '::' qualtype
                    {% doForeign $1 (vu_import:$3) $4 $6  }
    | 'foreign' wl_var mstring '::' qualtype {% doForeign $1 $2 $3 $5  }
    | propspragma srcloc ecl_var '#-}'  { HsPragmaProps $2 $1 $3 }
    | 'deriving' 'instance' classhead { HsDeclDeriving $1 $3 }
    | 'default' type { HsDefaultDecl $1 $2 }
    | rulecatalyst rules '#-}' {
        HsPragmaRules $ map (\x -> x { hsRuleIsMeta = $1 }) (reverse $2) }
    | srcloc specialize m_con var '::' type '#-}'
                      { HsPragmaSpecialize { hsDeclSrcLoc = $1, hsDeclBool = $2, hsDeclName = $4, hsDeclType = $6
                                           , hsDeclUniq = error "hsDeclUniq not set"  } }
#maybe con

rulecatalyst ::  { Bool }
    : 'RULE' { False }
    | 'CATALYST' { True }
specialize ::  { Bool }
    : 'SPECIALIZE' { False }
    | 'SUPERSPECIALIZE' { True }

rule :: { HsRule } : srcloc STRING mfreevars exp '=' exp { HsRule {
    hsRuleSrcLoc = $1,
    hsRuleString = $2,
    hsRuleFreeVars = $3,
    hsRuleLeftExpr = $4,
    hsRuleRightExpr = $6,
    hsRuleUniq = error "hsRuleUniq not set",
    hsRuleIsMeta = error "hsRuleIsMeta not set" } }

mfreevars :: { [(HsName,Maybe HsType)] }
      : 'forall' vbinds '.' { $2 }
      | { [] }

vbinds :: { [(HsName,Maybe HsType)] }
      : vbinds '(' var '::' type ')' { ($3,Just $5) : $1 }
      | vbinds var                   { ($2,Nothing) : $1 }
      |                              { [] }

propspragma :: { String }
    : 'INLINE' { $1 }
    | 'MULTISPECIALIZE' { $1 }
    | 'NOINLINE' { $1 }
    | 'SRCLOC_ANNOTATE' { $1 }
    | 'SUPERINLINE' { $1 }
    | 'NOETA' { $1 }

-- FFI parts
mstring :: { Maybe (String,Name) }
mstring : LString var    { Just (read $1,$2) }
        | {- empty -}    { Nothing }

classhead :: { HsClassHead }
    : qualtype {% qualTypeToClassHead $1 }

mconstrs :: { ([HsConDecl],Maybe HsKind) }
    : '=' bl_constr  { ($2,Nothing) }
    | '::' kind      { ([],Just $2) }
    |                { ([],Nothing) }

kind :: { HsKind }
      : bkind                          { $1 }
      | bkind '->' kind                { HsKindFn $1 $3 }

bkind :: { HsKind }
       : '(' kind ')'     { $2 }
       |  varop           {% toKindVarSym $1 }
       |  con             { HsKind $ nameTyLevel_s kindLevel $1 }

deriving :: { [Name] }
    : {- empty -}               { [] }
    | 'deriving' con            { [toName ClassName $2] }
    | 'deriving' '(' ')'        { [] }
    | 'deriving' '(' cl_con ')' { map (toName ClassName) $3 }

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
    : cl_var '::' wl_batype  {% withSrcLoc $2 $ do
        tty <- checkBangType $3
        return (map (toName FieldLabel) $1, tty) }

mexists :: { [HsTyVarBind] }
        : 'exists' wl_tbind '.' { $2 }
        | 'forall' wl_tbind '.' { $2 }  -- Allowed for GHC compatability
        |                       { [] }

scontype :: { (HsName, [HsBangType]) }
    : wl_batype {% checkSconType $1  }

batype :: { Either Name HsType }
    : varconop    { Left $1 }
    | '->'        { Left tc_Arrow }
    | atype       { Right $1 }
    | quantifiedtype { Right $1 }

rhs :: { HsRhs }
    : '=' exp   { HsUnGuardedRhs $2 }
    | wl_gdrh   { HsGuardedRhss $1 }

gdrh :: { HsGuardedRhs }
      : '|' exp '=' exp        { HsGuardedRhs $1 $2 $4 }

rhs_case :: { HsRhs }
    : '->' exp      { HsUnGuardedRhs $2 }
    | wl_gdrh_case  { HsGuardedRhss $1 }

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
    : LQConId { toModule $1 }
    | LConId { toModule $1 }

optqualified :: { Bool }
    : 'qualified'   { True  }
    | {- empty -}   { False }

maybeas :: { Maybe Module }
    : 'as' modid    { Just $2 }
    | {- empty -}   { Nothing }

maybeimpspec :: { Maybe (Bool, [HsExportSpec]) }
    : exports             { Just (False,$1) }
    | 'hiding' exports    { Just (True,$2) }
    | {- empty -}         { Nothing }

exports :: { [HsExportSpec] }
--    : '(' ')'                     { [] }
    : '(' ocl_export ')'  { $2 }

export :: { HsExportSpec }
    :  var                          { HsEVar $1 }
    |  con                          { HsEAbs $1 }
    |  con '(' '..' ')'             { HsEThingAll $1 }
    |  con '(' ecl_varcon ')'       { HsEThingWith $1 $3 }
    |  'module' modid               { HsEModuleContents $2 }

quantifiedtype :: { HsType }
    : 'forall' wl_tbind '.' qualtype  { HsTyForall { hsTypeVars = $2, hsTypeType = $4 } }
    | 'exists' wl_tbind '.' qualtype  { HsTyExists { hsTypeVars = $2, hsTypeType = $4 } }

type :: { HsType }
    : btype '->' type               { HsTyFun $1 $3 }
    | btype                         { $1 }
    | quantifiedtype                { $1 }

tbind :: { HsTyVarBind }
       : srcloc var                   { hsTyVarBind { hsTyVarBindSrcLoc = $1, hsTyVarBindName = nameTyLevel_s typeLevel $2 } }
       | srcloc '(' var '::' kind ')' { hsTyVarBind { hsTyVarBindSrcLoc = $1, hsTyVarBindName = nameTyLevel_s typeLevel $3, hsTyVarBindKind = Just $5 } }

btype :: { HsType }
    : btype atype                   { HsTyApp $1 $2 }
    | atype                         { $1 }

atype :: { HsType }
    : gcon                   { HsTyCon (nameTyLevel_s typeLevel $1) }
    | var                    { HsTyVar (nameTyLevel_s typeLevel $1) }
    | '(' '->' ')'           { HsTyCon $ quoteName tc_Arrow }
    | '(' ')'                { HsTyTuple [] }
    | '[' ']'                { HsTyCon $ quoteName tc_List }
    | '(' cl2_type ')'       { HsTyTuple $2 }
    | '(#' ecl_type '#)'     { HsTyUnboxedTuple $2 }
    | '[' type ']'           {% do
        return $ HsTyApp (HsTyCon $ quoteName tc_List) $2 }
    | '(' type ')'           { $2 }
    | '(' type '=' type ')'  { HsTyEq $2 $4 }

qualtype :: { HsQualType }
    : btype '=>' type   {% withSrcLoc $2 $ checkContext $1 >>= return . flip HsQualType $3 }
    | type             { HsQualType [] $1 }

exp :: { HsExp }
    : exp0 '::' qualtype { HsExpTypeSig $2 $1 $3 }
    | exp0               { $1 }

exp0  :: { HsExp }
    : exp1 { $1 }
    | aexp exp0 { $1 `cat` $2 }

exp1 :: { HsExp }
    : 'if' exp 'then' exp 'else' exp { HsIf (espan $1 $3 $ $2) (espan $3 $5 $4) (eloc $5 $6) }
    | '\\' pats '->' exp { HsLambda $1 $2 $4 }
    | 'let' '{' decls '}' 'in' exp { HsLet (fixupHsDecls $3) $6 }
    | 'case' exp 'of' '{' alts '}'  { espan $1 $6 $ HsCase (espan $1 $3 $2) $5 }
    | aexp  { $1 }

stmt :: { HsStmt }
    : epat '<-' exp      { HsGenerator $2 $1 $3 }
    | exp               { HsQualifier $1 }
    | 'let' '{' decls '}'    { HsLetStmt  (fixupHsDecls $3) }

alt :: { HsAlt }
    : srcloc epat rhs_case optwhere { HsAlt $1 $2 $3 $4 }
 --   : pat '->' exp { HsAlt $2 $1 (HsUnGuardedRhs $3) [] }

aexp :: { HsExp }
    : '(' ecl_exp ')'   {% do
        let ee = espan $1 $3
        case $2 of
            [x] -> return $ ee (HsParen x)
            []  -> return (HsCon $ quoteName dc_Unit)
            xs -> return $ ee $ HsTuple xs }
    | '(#' ecl_exp '#)' { espan $1 $3 $ HsUnboxedTuple $2 }
    | '[' list ']'      { espan $1 $3 $2 }
    | '_'               { HsWildCard $1 }
    | var               { HsVar $1 }
    | gcon              { HsCon $1 }
    | varop             { HsBackTick (HsVar $1) }
    | conop             { HsBackTick (HsCon $1) }
    | lit               { HsLit $1 }
    -- atomic after layout processing
    | 'do' '{' stmts  '}'    { HsDo $3 }
    | aexp '{' ecl_fbind '}' {% mkRecConstrOrUpdate $1 $3 }

m_comma :: { () } : ',' { () } |  { () }

commas :: { Int }
    : commas ','                    { $1 + 1 }
    | ','                           { 1 }

fbind :: { (Name,Maybe HsExp) }
    : uqvar '=' exp  { (toName FieldLabel $1,Just (eloc $2 $3)) }
    | uqvar { (toName FieldLabel $1,Nothing) }
    | '..'  { (u_DotDot,Nothing) }

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

lit :: { HsLiteral }
    : LInteger  { HsInt $ read $1 }
    | LChar     { HsChar $ read $1 }
    | LString   { HsString $ read $1 }
    | LFloat    { HsFrac  $ toRational (read $1 :: Double) }

    | LChar_     { HsCharPrim $ readPrim $1 }
    | LFloat_    { HsFrac  $ toRational (readPrim $1 :: Double) }
    | LInteger_  { HsIntPrim $ readPrim $1 }
    | LString_   { HsStringPrim $ readPrim $1 }

STRING :: { String } : LString { read $1 }
INT :: { Int } : LInteger { read $1 }

#map { clist $_ } qw/exp stmt type var varcon varconop export con fielddecl fbind/
#blist constr
#map { wlist $_ } qw/aexp var gdrh batype gdrh_case tbind/
#ewlist aexp
#ewlist atype
#ewlist var

#[def oslist   qq[
${a}s : rev_$a { reverse \$1 }
rev_$a
    : rev_$a ';' $a { \$3 : \$1 }
    | rev_$a ';'    { \$1 }
    | $a            { [\$1] }
    | {- empty -}   { [] }
]
#]
#[def oclist   qq[
ocl_${a} : ocrev_$a { reverse \$1 }
ocrev_$a
    : ocrev_$a ',' $a { \$3 : \$1 }
    | ocrev_$a ','    { \$1 }
    | $a            { [\$1] }
    | {- empty -}   { [] }
]
#]

#oclist 'export'
#oslist 'decl'
#oslist 'impdecl'
#oslist 'alt'
#oslist 'stmt'
#oslist 'rule'

var :: { Name }
    : uqvar { $1 }
    | LQVarId  {(parseName Val $1) }

uqvar :: { Name }
    : LVarId  { (toName Val $1) }
    | 'as'                  { vu_as }
    | 'family'              { vu_family }
    | 'hiding'              { vu_hiding }
    | 'qualified'           { vu_qualified }

    | 'alias'               { vu_alias }
    | 'kind'                { vu_kind }
    | 'closed'              { vu_closed }
--    | 'forall'              { u_forall }
--    | 'exists'              { u_exists }

gcon :: { Name }
    : '(' commas ')'   { tuple_con_name $2 }
    | con              { $1 }

con :: { Name }
    : LConId  { (toName DataConstructor $1) }
    | LQConId { (parseName DataConstructor $1) }
    | cons_id { quoteName dc_Cons }

varcon
    : var { $1 }
    | con { $1 }

varconop
    : varop { $1 }
    | conop { $1 }

conop :: { Name }
    : LConSym  { (toName DataConstructor $1) }
    | LQConSym { (parseName DataConstructor $1) }
--    | ':'     {% do implicitName dc_Cons }
    | cons_sym { quoteName dc_Cons }

varop :: { Name }
    : LVarSym  { (toName Val $1) }
    | LQVarSym { (parseName Val $1) }
    | '.'      { vu_Dot }
--    | '~'      { vu_Twiddle }
--    | '@'      { vu_At }

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

x `cat` HsWords ws = HsWords (x:ws)
x `cat` y = HsWords [x,y]

eloc p e =  HsLocatedExp (Located (srcSpan p) e)
espan p1 p2 e =  HsLocatedExp (Located (SrcSpan p1 p2) e)
withSpan p1 p2 e =  withSrcSpan (SrcSpan p1 p2) e

tuple_con_name i = quoteName $ name_TupleConstructor termLevel (i + 1)

readPrim :: Read a => String -> a
readPrim s = case reads s of
    ~[(v,"#")] -> v

toKindVarSym n
    | Just k <- Map.lookup n kmap = return k
    | otherwise = parseErrorK $ "invalid kind: " ++ show n
    where kmap = Map.fromList
            [(vu_Star, hsKindStar)
            ,(vu_Hash, hsKindHash)
            ,(vu_Bang, hsKindBang)
            ,(vu_StarBang, hsKindStarBang)
            ,(vu_Quest, hsKindQuest)
            ,(vu_QuestQuest, hsKindQuestQuest)]

}
