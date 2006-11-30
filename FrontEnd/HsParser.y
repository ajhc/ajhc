-- -----------------------------------------------------------------------------
-- $Id: HsParser.ly,v 1.4 2001/11/25 08:52:13 bjpop Exp $

-- (c) Simon Marlow, Sven Panne 1997-2000
-- Modified by John Meacham

-- Haskell grammar.
-- -----------------------------------------------------------------------------

-- ToDo: Is (,) valid as exports? We don't allow it.
-- ToDo: Check exactly which names must be qualified with Prelude (commas and friends)
-- ToDo: Inst (MPCs?)
-- ToDo: Polish constr a bit
-- ToDo: Ugly: infixexp is used for lhs, pat, exp0, ...
-- ToDo: Differentiate between record updates and labeled construction.

{
module FrontEnd.HsParser (parse, parseHsStmt) where

import C.FFI
import HsSyn
import FrontEnd.ParseMonad
import FrontEnd.Lexer
import FrontEnd.ParseUtils hiding(readInteger,readRational)
import FrontEnd.SrcLoc



}

-- -----------------------------------------------------------------------------
-- Conflicts: 10 shift/reduce

-- 7 for abiguity in 'if x then y else z + 1'
--      (shift parses as 'if x then y else (z + 1)', as per longest-parse rule)
-- 1 for ambiguity in 'if x then y else z :: T'
--      (shift parses as 'if x then y else (z :: T)', as per longest-parse rule)
-- 2 for ambiguity in 'case x of y :: a -> b'
--      (don't know whether to reduce 'a' as a btype or shift the '->'.
--       conclusion:  bogus expression anyway, doesn't matter)

-- -----------------------------------------------------------------------------

%token
      VARID    { VarId $$ }
      QVARID   { QVarId $$ }
      CONID    { ConId $$ }
      QCONID   { QConId $$ }
      VARSYM   { VarSym $$ }
      CONSYM   { ConSym $$ }
      QVARSYM  { QVarSym $$ }
      QCONSYM  { QConSym $$ }
      INT      { IntTok $$ }
      UINT     { UIntTok $$ }
      RATIONAL { FloatTok $$ }
      CHAR     { Character $$ }
      UCHAR    { UCharacter $$ }
      STRING   { StringTok $$ }
      USTRING  { UStringTok $$ }
      PRAGMAOPTIONS { PragmaOptions $$ }
      PRAGMASTART { PragmaStart $$ }
      PRAGMARULES { PragmaRules $$ }
      PRAGMASPECIALIZE { PragmaSpecialize $$ }
      PRAGMAEND { PragmaEnd }

-- Symbols

      '('     { LeftParen }
      ')'     { RightParen }
      '(#'    { LeftUParen }
      '#)'    { RightUParen }
      ';'     { SemiColon }
      '{'     { LeftCurly }
      '}'     { RightCurly }
      vccurly { VRightCurly }                 -- a virtual close brace
      '['     { LeftSquare }
      ']'     { RightSquare }
      ','     { Comma }
      '_'     { Underscore }
      '`'     { BackQuote }

-- Reserved operators

      '..'    { DotDot }
      '::'    { DoubleColon }
      '='     { Equals }
      '\\'    { Backslash }
      '|'     { Bar }
      '<-'    { LeftArrow }
      '->'    { RightArrow }
      '@'     { At }
      '~'     { Tilde }
      '=>'    { DoubleArrow }
      '-'     { Minus }
      '!'     { Exclamation }
      '??'    { QuestQuest }
      '*!'    { StarBang }
      '*'     { Star }
      '#'     { Hash }
      '.'     { Dot }

-- Reserved Ids

      'as'            { KW_As }
      'case'          { KW_Case }
      'class'         { KW_Class }
      'data'          { KW_Data }
      'default'       { KW_Default }
      'deriving'      { KW_Deriving }
      'do'            { KW_Do }
      'else'          { KW_Else }
      'hiding'        { KW_Hiding }
      'if'            { KW_If }
      'import'        { KW_Import }
      'in'            { KW_In }
      'infix'         { KW_Infix }
      'infixl'        { KW_InfixL }
      'infixr'        { KW_InfixR }
      'instance'      { KW_Instance }
      'let'           { KW_Let }
      'module'        { KW_Module }
      'newtype'       { KW_NewType }
      'of'            { KW_Of }
      'then'          { KW_Then }
      'type'          { KW_Type }
      'where'         { KW_Where }
      'qualified'     { KW_Qualified }
      'foreign'       { KW_Foreign }
      'forall'        { KW_Forall }
      'exists'        { KW_Exists }
      'kind'          { KW_Kind }

%monad { P } { thenP } { returnP }
%lexer { lexer } { EOF }
%name parse module
%name parseHsStmt qual
%tokentype { Token }
%%

-- -----------------------------------------------------------------------------
-- Module Header
module :: { HsModule }
      : srcloc modulep                  { $2 { hsModuleSrcLoc = $1, hsModuleOptions = [] } }
      | srcloc PRAGMAOPTIONS module     { $3 { hsModuleSrcLoc = $1, hsModuleOptions = hsModuleOptions $3 ++ $2 } }

modulep  :: { HsModule }
      : 'module' modid maybeexports 'where' body      { HsModule { hsModuleName = $2, hsModuleExports = $3, hsModuleImports = (fst $5), hsModuleDecls = (snd $5) } }
      | body                                          { HsModule { hsModuleName = main_mod, hsModuleExports = Just [HsEVar (UnQual (HsIdent "main"))], hsModuleImports = (fst $1), hsModuleDecls = (snd $1) } }

body :: { ([HsImportDecl],[HsDecl]) }
      :  '{' bodyaux '}'                              { $2 }
      |      layout_on  bodyaux close                 { $2 }

bodyaux :: { ([HsImportDecl],[HsDecl]) }
      : impdecls ';' topdecls optsemi                 { (reverse $1, fixupHsDecls (reverse $3)) }
      |              topdecls optsemi                 { ([], fixupHsDecls (reverse $1)) }
      | impdecls              optsemi                 { (reverse $1, []) }
      | {- empty -}                                   { ([], []) }

optsemi :: { () }
      : ';'                                           { () }
      | {- empty -}                                   { () }

-- -----------------------------------------------------------------------------
-- The Export List

maybeexports :: { Maybe [HsExportSpec] }
      :  exports                              { Just $1 }
      |  {- empty -}                          { Nothing }

exports :: { [HsExportSpec] }
      : '(' exportlist maybecomma ')'         { reverse $2 }
      | '(' ')'                               { [] }

maybecomma :: { () }
      : ','                                   { () }
      | {- empty -}                           { () }

exportlist :: { [HsExportSpec] }
      :  exportlist ',' export                { $3 : $1 }
      |  export                               { [$1]  }

export :: { HsExportSpec }
      :  qvar                                 { HsEVar $1 }
      |  qtyconorcls                          { HsEAbs $1 }
      |  qtyconorcls '(' '..' ')'             { HsEThingAll $1 }
      |  qtyconorcls '(' ')'                  { HsEThingWith $1 [] }
      |  qtyconorcls '(' qcnames ')'          { HsEThingWith $1 (reverse $3) }
      |  'module' modid                       { HsEModuleContents $2 }

qcnames :: { [HsName] }
      :  qcnames ',' qcname                   { $3 : $1 }
      |  qcname                               { [$1]  }

qcname :: { HsName }
      :  qvar                                 { $1 }
      |  qcon                                 { $1 }

-- -----------------------------------------------------------------------------
-- Import Declarations

impdecls :: { [HsImportDecl] }
      : impdecls ';' impdecl                  { $3 : $1 }
      | impdecl                               { [$1] }

impdecl :: { HsImportDecl }
      : 'import' srcloc optqualified modid maybeas maybeimpspec
                              { HsImportDecl $2 $4 $3 $5 $6 }

optqualified :: { Bool }
      : 'qualified'                           { True  }
      | {- empty -}                           { False }

maybeas :: { Maybe Module }
      : 'as' modid                            { Just $2 }
      | {- empty -}                           { Nothing }


maybeimpspec :: { Maybe (Bool, [HsImportSpec]) }
      : impspec                               { Just $1 }
      | {- empty -}                           { Nothing }

impspec :: { (Bool, [HsImportSpec]) }
      :  '(' importlist maybecomma ')'        { (False, reverse $2) }
      |  'hiding' '(' importlist maybecomma ')' { (True,  reverse $3) }

importlist :: { [HsImportSpec] }
      :  importlist ',' import                { $3 : $1 }
      |  import                               { [$1]  }

import :: { HsImportSpec }
      :  var                                  { HsIVar $1 }
      |  tyconorcls                           { HsIAbs $1 }
      |  tyconorcls '(' '..' ')'              { HsIThingAll $1 }
      |  tyconorcls '(' ')'                   { HsIThingWith $1 [] }
      |  tyconorcls '(' cnames ')'            { HsIThingWith $1 (reverse $3) }

cnames :: { [HsName] }
      :  cnames ',' cname                     { $3 : $1 }
      |  cname                                { [$1]  }

cname :: { HsName }
      :  var                                  { $1 }
      |  con                                  { $1 }

-- -----------------------------------------------------------------------------
-- Fixity Declarations

fixdecl :: { HsDecl }
      : srcloc infix prec ops                 { HsInfixDecl $1 $2 $3 (reverse $4) }

prec :: { Int }
      : {- empty -}                           { 9 }
      | INT                                   {%  checkPrec $1 `thenP` \p ->
                                                  returnP (fromInteger (readInteger p)) }

infix :: { HsAssoc }
      : 'infix'                               { HsAssocNone  }
      | 'infixl'                              { HsAssocLeft  }
      | 'infixr'                              { HsAssocRight }

ops   :: { [HsName] }
      : ops ',' op                            { $3 : $1 }
      | op                                    { [$1] }

-- -----------------------------------------------------------------------------
-- Top-Level Declarations

-- Note: The report allows topdecls to be empty. This would result in another
-- shift/reduce-conflict, so we don't handle this case here, but in bodyaux.

topdecls :: { [HsDecl] }
      : topdecls ';' topdecl          { $3 : $1 }
      | topdecl                       { [$1] }

topdecl :: { HsDecl }
      : 'data' ctype srcloc deriving
          {% checkDataHeader $2 `thenP` \(cs,c,t) ->
             returnP hsDataDecl { hsDeclSrcLoc = $3, hsDeclContext = cs, hsDeclName = c, hsDeclArgs = t, hsDeclDerives = $4 } }
      | 'data' ctype '::' kind srcloc deriving
          {% checkDataHeader $2 `thenP` \(cs,c,t) ->
             returnP hsDataDecl { hsDeclSrcLoc = $5, hsDeclContext = cs, hsDeclName = c, hsDeclArgs = t, hsDeclDerives = $6, hsDeclHasKind = Just $4 } }
      | 'data' ctype srcloc '=' constrs deriving
                      {% checkDataHeader $2 `thenP` \(cs,c,t) ->
                         returnP hsDataDecl { hsDeclSrcLoc = $3, hsDeclContext = cs, hsDeclName = c, hsDeclArgs = t, hsDeclDerives = $6, hsDeclCons = reverse $5 } }
      | 'data' 'kind' ctype srcloc '=' constrs deriving
                      {% checkDataHeader $3 `thenP` \(cs,c,t) ->
                         returnP hsDataDecl { hsDeclKindDecl = True, hsDeclSrcLoc = $4, hsDeclContext = cs, hsDeclName = c, hsDeclArgs = t, hsDeclDerives = $7, hsDeclCons = reverse $6 } }
      | 'newtype' ctype srcloc '=' constr deriving
                      {% checkDataHeader $2 `thenP` \(cs,c,t) ->
                         returnP (HsNewTypeDecl $3 cs c t $5 $6) }
      | 'class' srcloc ctype optfundep optcbody
                      { HsClassDecl $2 $3 $5 }
      | 'instance' srcloc ctype optvaldefs
                      { HsInstDecl $2 $3 $4 }
      | 'default' srcloc type
                      { HsDefaultDecl $2 $3 }
      | infixexp srcloc '<-' exp      {% checkPattern $1 `thenP` \p ->
                                         returnP (HsActionDecl $2 p $4) }
      | 'foreign' srcloc 'import' varids mstring '::' ctype
                      {% doForeign $2 (UnQual (HsIdent "import"):reverse $4) $5 $7  }
      | 'foreign' srcloc varids mstring '::' ctype
                      {% doForeign $2 (reverse $3) $4 $6  }
      | 'foreign' srcloc varids mstring '::' ctype '=' exp
                      {% doForeignEq $2 (reverse $3) $4 $6 $8 }
      | PRAGMARULES layout_on rules close PRAGMAEND
              { HsPragmaRules $ map (\x -> x { hsRuleIsMeta = $1 }) (reverse $3) }
      | srcloc PRAGMASPECIALIZE var '::' type PRAGMAEND
                      { HsPragmaSpecialize { hsDeclSrcLoc = $1, hsDeclBool = $2, hsDeclName = $3, hsDeclType = $5 } }
      | decl          { $1 }

rule :: { HsRule }
      : srcloc STRING mfreevars exp '=' exp
         { HsRule { hsRuleSrcLoc = $1, hsRuleString = $2, hsRuleFreeVars = $3, hsRuleLeftExpr = $4, hsRuleRightExpr = $6 } }

rules :: { [HsRule] }
      : rules optsemi rule  { $3 : $1 }
      | rule optsemi           { [$1] }


mfreevars :: { [(HsName,Maybe HsType)] }
      : 'forall' vbinds '.' { $2 }
      | { [] }

vbinds :: { [(HsName,Maybe HsType)] }
      : vbinds '(' var '::' type ')' { ($3,Just $5) : $1 }
      | vbinds var                   { ($2,Nothing) : $1 }
      |                              { [] }

decls :: { [HsDecl] }
      : decls1 optsemi                { fixupHsDecls ( reverse $1 ) }
      | optsemi                       { [] }

decls1 :: { [HsDecl] }
      : decls1 ';' decl               { $3 : $1 }
      | decl                          { [$1] }

decl :: { HsDecl }
      : signdecl                      { $1 }
      | fixdecl                       { $1 }
      | valdef                        { $1 }
      | pragmaprops                   { $1 }



decllist :: { [HsDecl] }
      : '{' decls '}'                 { $2 }
      |     layout_on  decls close    { $2 }

signdecl :: { HsDecl }
      : vars srcloc '::' ctype        { HsTypeSig $2 (reverse $1) $4 }

pragmaprops  :: { HsDecl }
      : PRAGMASTART srcloc  vars PRAGMAEND  { HsPragmaProps $2 $1 $3 }

-- ATTENTION: Dirty Hackery Ahead! If the second alternative of vars is var
-- instead of qvar, we get another shift/reduce-conflict. Consider the
-- following programs:

--    { (+) :: ... }          only var
--    { (+) x y  = ... }      could (incorrectly) be qvar

-- We re-use expressions for patterns, so a qvar would be allowed in patterns
-- instead of a var only (which would be correct). But deciding what the + is,
-- would require more lookahead. So let's check for ourselves...

vars  :: { [HsName] }
      : vars ',' var                  { $3 : $1 }
      | qvar                          {% checkUnQual $1 `thenP` \n ->
                                         returnP [n] }

-- FFI parts
{-
mcconv :: { CallConv }
mcconv : 'ccall'        { CCall }
       | 'stdcall'      { StdCall }
       | {- empty -}    { CCall }

msafety :: { Safety }
msafety : 'safe'        { Safe }
        | 'unsafe'      { Unsafe }
        | {- empty -}   { Safe }
-}
mstring :: { Maybe (String,HsName) }
mstring : STRING var        { Just ($1,$2) }
        | {- empty -}    { Nothing }

-- -----------------------------------------------------------------------------
-- Types

type :: { HsType }
      : btype '->' type               { HsTyFun $1 $3 }
      | btype                         { $1 }
      | 'forall' tbinds '.' ctype     { HsTyForall { hsTypeVars = reverse $2, hsTypeType = $4 } }
      | 'exists' tbinds '.' ctype     { HsTyExists { hsTypeVars = reverse $2, hsTypeType = $4 } }

tbinds :: { [HsTyVarBind] }
      : tbinds tbind                  { $2 : $1 }
      | tbind                         { [$1] }

tbind :: { HsTyVarBind }
       : srcloc varid                   { hsTyVarBind { hsTyVarBindSrcLoc = $1, hsTyVarBindName = $2 } }
       | srcloc '(' varid '::' kind ')' { hsTyVarBind { hsTyVarBindSrcLoc = $1, hsTyVarBindName = $3, hsTyVarBindKind = Just $5 } }

kind :: { HsKind }
      : bkind                          { $1 }
      | bkind '->' kind                { HsKindFn $1 $3 }

bkind :: { HsKind }
       : '(' kind ')'           { $2 }
       |  '*'                   { hsKindStar }
       |  '#'                   { hsKindHash }
       |  '!'                   { hsKindBang }
       |  '*!'                  { hsKindStarBang }
       |  '??'                  { hsKindQuestQuest }
       |  qconid                { HsKind $1 }

btype :: { HsType }
      : btype atype                   { HsTyApp $1 $2 }
      | atype                         { $1 }

atype :: { HsType }
      : gtycon                        { HsTyCon $1 }
      | tyvar                         { HsTyVar $1 }
      | '(' types ')'                 { HsTyTuple (reverse $2) }
      | '(#' '#)'                     { HsTyUnboxedTuple [] }
      | '(#' type '#)'                { HsTyUnboxedTuple [$2] }
      | '(#' types '#)'               { HsTyUnboxedTuple (reverse $2) }
      | '[' type ']'                  { HsTyApp list_tycon $2 }
      | '(' ktype ')'                 { $2 }
      | '(' type '=' type ')'         { HsTyEq $2 $4 }

ktype :: { HsType }
    : srcloc atype '::' kind { HsTyExpKind { hsTySrcLoc = $1, hsTyType = $2, hsTyKind = $4 } }
    | type                  { $1 }

gtycon :: { HsName }
      : qconid                        { $1 }
      | '(' ')'                       { unit_tycon_name }
      | '(' '->' ')'                  { fun_tycon_name }
      | '[' ']'                       { list_tycon_name }
      | '(' commas ')'                { tuple_tycon_name $2 }


-- (Slightly edited) Comment from GHC's hsparser.y:
-- "context => type" vs  "type" is a problem, because you can't distinguish between

--      foo :: (Baz a, Baz a)
--      bar :: (Baz a, Baz a) => [a] -> [a] -> [a]

-- with one token of lookahead.  The HACK is to parse the context as a btype
-- (more specifically as a tuple type), then check that it has the right form
-- C a, or (C1 a, C2 b, ... Cn z) and convert it into a context.  Blaach!

ctype :: { HsQualType }
      : btype '=>' type               {% checkContext $1 `thenP` \c ->
                                         returnP (HsQualType c $3) }
      | type                          { HsQualType [] $1 }

types :: { [HsType] }
      : types ',' type                { $3 : $1 }
      | type  ',' type                { [$3, $1] }

simpletype :: { (HsName, [HsType]) }
      : tycon atypes                  { ($1,reverse $2) }

atypes :: { [HsType] }
      : atypes atype                  { $2 : $1 }
      | {- empty -}                   { [] }

-- -----------------------------------------------------------------------------
-- Datatype declarations

constrs :: { [HsConDecl] }
      : constrs '|' constr            { $3 : $1 }
      | constr                        { [$1] }

constr :: { HsConDecl }
      : srcloc mexists scontype               { HsConDecl { hsConDeclSrcLoc = $1, hsConDeclName = (fst $3), hsConDeclConArg = (snd $3), hsConDeclExists = $2 } }
      | srcloc mexists sbtype conop sbtype    { HsConDecl { hsConDeclSrcLoc = $1, hsConDeclName = $4, hsConDeclConArg = [$3,$5], hsConDeclExists = $2 } }
      | srcloc mexists con '{' fielddecls '}'
                                      { HsRecDecl { hsConDeclSrcLoc = $1, hsConDeclName = $3, hsConDeclRecArg = (reverse $5), hsConDeclExists = $2 } }

mexists :: { [HsTyVarBind] }
        : 'exists' tbinds '.'         { $2 }
        | 'forall' tbinds '.'         { $2 }  -- Allowed for GHC compatability
        |                             { [] }

scontype :: { (HsName, [HsBangType]) }
      : btype                         {% splitTyConApp $1 `thenP` \(c,ts) ->
                                         returnP (c,map HsUnBangedTy ts) }
      | scontype1                     { $1 }

scontype1 :: { (HsName, [HsBangType]) }
      : btype '!' atype               {% splitTyConApp $1 `thenP` \(c,ts) ->
                                         returnP (c,map HsUnBangedTy ts++
                                                      [HsBangedTy $3]) }
      | scontype1 satype              { (fst $1, snd $1 ++ [$2] ) }

satype :: { HsBangType }
      : atype                         { HsUnBangedTy $1 }
      | '!' atype                     { HsBangedTy   $2 }

sbtype :: { HsBangType }
      : btype                         { HsUnBangedTy $1 }
      | '!' atype                     { HsBangedTy   $2 }

fielddecls :: { [([HsName],HsBangType)] }
      : fielddecls ',' fielddecl      { $3 : $1 }
      | fielddecl                     { [$1] }

fielddecl :: { ([HsName],HsBangType) }
      : vars '::' stype               { (reverse $1, $3) }

stype :: { HsBangType }
      : type                          { HsUnBangedTy $1 }
      | '!' atype                     { HsBangedTy   $2 }

deriving :: { [HsName] }
      : {- empty -}                   { [] }
      | 'deriving' qtycls             { [$2] }
      | 'deriving' '('          ')'   { [] }
      | 'deriving' '(' dclasses ')'   { reverse $3 }

dclasses :: { [HsName] }
      : dclasses ',' qtycls           { $3 : $1 }
      | qtycls                        { [$1] }

-- -----------------------------------------------------------------------------
-- Class declarations

optcbody :: { [HsDecl] }
      : 'where' decllist                      { fixupHsDecls $2 }
      | {- empty -}                           { [] }

cdefaults :: { [HsDecl] }
     : cdefaults ';' valdef                  { $3 : $1 }
     | valdef                                { [$1] }

-- -----------------------------------------------------------------------------
-- Functional dependencies

optfundep :: { [([HsName],[HsName])] }
      : {- empty -}                           { [] }
      | '|' fundeps                           { reverse $2 }

fundeps   :: { [([HsName],[HsName])] }
      : fundeps ',' fundep                    { ($3:$1) }
      | fundep                                { [$1]    }

fundep    :: { ([HsName],[HsName]) }
      : varids '->' varids                    { ($1,$3) }

varids    :: { [HsName] }
      : {- empty -}                           { [] }
      | varids varid                          { ($2:$1) }

-- -----------------------------------------------------------------------------
-- Instance declarations

optvaldefs :: { [HsDecl] }
      : 'where' '{' valdefs '}'               { $3 }
      | 'where' layout_on valdefs close       { $3 }
      | {- empty -}                           { [] }

-- Recycling...

valdefs :: { [HsDecl] }
      : cdefaults optsemi                     { fixupHsDecls (reverse $1) }
      | optsemi                               { [] }

-- -----------------------------------------------------------------------------
-- Value definitions

valdef :: { HsDecl }
      : 'type' simpletype srcloc '=' type
                      { HsTypeDecl $3 (fst $2) (snd $2) $5 }
      | 'type' simpletype srcloc
                      { HsTypeDecl $3 (fst $2) (snd $2) HsTyAssoc }
      | infixexp srcloc rhs                   {% checkValDef $2 $1 $3 []}
      | infixexp srcloc rhs 'where' decllist  {% checkValDef $2 $1 $3 $5}

rhs   :: { HsRhs }
      : '=' exp                       {% checkExpr $2 `thenP` \e ->
                                         returnP (HsUnGuardedRhs e) }
      | gdrhs                         { HsGuardedRhss  (reverse $1) }

gdrhs :: { [HsGuardedRhs] }
      : gdrhs gdrh                    { $2 : $1 }
      | gdrh                          { [$1] }

gdrh :: { HsGuardedRhs }
      : '|' exp srcloc '=' exp        {% checkExpr $2 `thenP` \g ->
                                         checkExpr $5 `thenP` \e ->
                                         returnP (HsGuardedRhs $3 g e) }

-- -----------------------------------------------------------------------------
-- Expressions

exp   :: { HsExp }
      : infixexp '::' srcloc ctype    { HsExpTypeSig $3 $1 $4 }
      | infixexp                      { $1 }

infixexp :: { HsExp }
      : exp10                         { $1 }
      | infixexp qop exp10            { HsInfixApp $1 $2 $3 }

exp10 :: { HsExp }
      : '\\' aexps srcloc '->' exp    {% checkPatterns (reverse $2) `thenP` \ps ->
                                         returnP (HsLambda $3 ps $5) }
      | 'let' decllist 'in' exp       { HsLet $2 $4 }
-- -- > | 'if' exp 'then' exp 'else' exp { HsIf $2 $4 $6 }
      | 'if' exp optsemi 'then' exp optsemi 'else' exp { HsIf $2 $5 $8 }
      | 'case' exp 'of' altslist      { HsCase $2 $4 }
      | '-' fexp                      { HsNegApp $2 }
      | 'do' stmtlist                 { HsDo $2 }
      | fexp                          { $1 }

fexp :: { HsExp }
      : fexp aexp                     { HsApp $1 $2 }
      | aexp                          { $1 }

aexps :: { [HsExp] }
      : aexps aexp                    { $2 : $1 }
      | aexp                          { [$1] }

-- UGLY: Because patterns and expressions are mixed, aexp has to be split into
-- two rules: One left-recursive and one right-recursive. Otherwise we get two
-- reduce/reduce-errors (for as-patterns and irrefutable patters).

-- Note: The first alternative of aexp is not neccessarily a record update, it
-- could be a labeled construction, too.

aexp  :: { HsExp }
      : aexp '{' fbinds '}'           {% mkRecConstrOrUpdate $1 (reverse $3) }
      | aexp1                         { $1 }

-- Even though the variable in an as-pattern cannot be qualified, we use
-- qvar here to avoid a shift/reduce conflict, and then check it ourselves
-- (as for vars above).

aexp1 :: { HsExp }
      : qvar                          { HsVar $1 }
      | gcon                          { $1 }
      | literal                       { $1 }
      | '(' exp ')'                   { HsParen $2 }
      | '(' texps ')'                 { HsTuple (reverse $2) }
      | '(#' '#)'                     { HsUnboxedTuple [] }
      | '(#' exp '#)'                 { HsUnboxedTuple [$2] }
      | '(#' texps '#)'               { HsUnboxedTuple (reverse $2) }
      | '[' list ']'                  { $2 }
      | '(' infixexp qop ')'          { HsLeftSection $3 $2  }
      | '(' qopm infixexp ')'         { HsRightSection $3 $2 }
      | qvar '@' aexp1                {% checkUnQual $1 `thenP` \n ->
                                         returnP (HsAsPat n $3) }
      | srcloc '_'                    { HsWildCard $1 }
      | '~' aexp1                     { HsIrrPat $2 }

commas :: { Int }
      : commas ','                    { $1 + 1 }
      | ','                           { 1 }

texps :: { [HsExp] }
      : texps ',' exp                 { $3 : $1 }
      | exp ',' exp                   { [$3,$1] }

-- -----------------------------------------------------------------------------
-- List expressions

-- The rules below are little bit contorted to keep lexps left-recursive while
-- avoiding another shift/reduce-conflict.

list :: { HsExp }
      : exp                           { HsList [$1] }
      | lexps                         { HsList (reverse $1) }
      | exp '..'                      { HsEnumFrom $1 }
      | exp ',' exp '..'              { HsEnumFromThen $1 $3 }
      | exp '..' exp                  { HsEnumFromTo $1 $3 }
      | exp ',' exp '..' exp          { HsEnumFromThenTo $1 $3 $5 }
      | exp '|' quals                 { HsListComp $1 (reverse $3) }

lexps :: { [HsExp] }
      : lexps ',' exp                 { $3 : $1 }
      | exp ',' exp                   { [$3,$1] }

-- -----------------------------------------------------------------------------
-- List comprehensions

quals :: { [HsStmt] }
      : quals ',' qual                        { $3 : $1 }
      | qual                                  { [$1] }

qual  :: { HsStmt }
      : infixexp srcloc '<-' exp      {% checkPattern $1 `thenP` \p ->
                                         returnP (HsGenerator $2 p $4) }
      | exp                           { HsQualifier $1 }
      | 'let' decllist                { HsLetStmt $2 }

-- -----------------------------------------------------------------------------
-- Case alternatives

altslist :: { [HsAlt] }
      : '{' alts optsemi '}'                  { reverse $2 }
      |     layout_on  alts optsemi close     { reverse $2 }


alts :: { [HsAlt] }
      : alts ';' alt                          { $3 : $1 }
      | alt                                   { [$1] }

alt :: { HsAlt }
      : infixexp srcloc ralt  {% checkPattern $1 `thenP` \p ->
                                 returnP (HsAlt $2 p $3 []) }
      | infixexp srcloc ralt 'where' decllist
                              {% checkPattern $1 `thenP` \p ->
                                 returnP (HsAlt $2 p $3 $5) }

ralt :: { HsRhs }
      : '->' exp                              { HsUnGuardedRhs $2 }
      | gdpats                                { HsGuardedRhss (reverse $1) }

gdpats :: { [HsGuardedRhs] }
      : gdpats gdpat                          { $2 : $1 }
      | gdpat                                 { [$1] }

gdpat :: { HsGuardedRhs }
      : '|' exp srcloc '->' exp               { HsGuardedRhs $3 $2 $5 }

-- -----------------------------------------------------------------------------
-- Statement sequences

stmtlist :: { [HsStmt] }
        : '{' stmts '}'               { $2 }
        |     layout_on  stmts close  { $2 }

stmts :: { [HsStmt] }
      : stmts1 ';' exp                { reverse (HsQualifier $3 : $1) }
      | exp                           { [HsQualifier $1] }

stmts1 :: { [HsStmt] }
      : stmts1 ';' qual               { $3 : $1 }
      | qual                          { [$1] }

-- -----------------------------------------------------------------------------
-- Record Field Update/Construction

fbinds :: { [HsFieldUpdate] }
      : fbinds ',' fbind              { $3 : $1 }
      | fbind                         { [$1] }

fbind :: { HsFieldUpdate }
      : qvar '=' exp                  { HsFieldUpdate $1 $3 }

-- -----------------------------------------------------------------------------
-- Variables, Constructors and Operators.

gcon :: { HsExp }
      : '(' ')'               { unit_con }
      | '[' ']'               { HsList [] }
      | '(' commas ')'        { tuple_con $2 }
      | qcon                  { HsCon $1 }

var   :: { HsName }
      : varid                 { $1 }
      | '(' varsym ')'        { $2 }

qvar  :: { HsName }
      : qvarid                { $1 }
      | '(' qvarsym ')'       { $2 }

con   :: { HsName }
      : conid                 { $1 }
      | '(' consym ')'        { $2 }

qcon  :: { HsName }
      : qconid                { $1 }
      | '(' qconsym ')'       { $2 }

varop :: { HsName }
      : varsym                { $1 }
      | '`' varid '`'         { $2 }

qvarop :: { HsName }
      : qvarsym               { $1 }
      | '`' qvarid '`'        { $2 }

qvaropm :: { HsName }
      : qvarsymm              { $1 }
      | '`' qvarid '`'        { $2 }

conop :: { HsName }
      : consym                { $1 }
      | '`' conid '`'         { $2 }

qconop :: { HsName }
      : qconsym               { $1 }
      | '`' qconid '`'        { $2 }

op    :: { HsName }
      : varop                 { $1 }
      | conop                 { $1 }

qop   :: { HsExp }
      : qvarop                { HsVar $1 }
      | qconop                { HsCon $1 }

qopm  :: { HsExp }
      : qvaropm               { HsVar $1 }
      | qconop                { HsCon $1 }

qvarid :: { HsName }
      : varid                 {  $1 }
      | QVARID                { Qual (Module (fst $1)) (HsIdent (snd $1)) }

varid :: { HsName }
      : VARID                 { UnQual (HsIdent $1) }
      | 'as'                  { as_name }
      | 'kind'                { UnQual (HsIdent "kind") }
      | 'qualified'           { qualified_name }
      | 'hiding'              { hiding_name }

qconid :: { HsName }
      : conid                 {  $1 }
      | QCONID                { Qual (Module (fst $1)) (HsIdent (snd $1)) }

conid :: { HsName }
      : CONID                 { UnQual (HsIdent $1) }

qconsym :: { HsName }
      : consym                {  $1 }
      | QCONSYM               { Qual (Module (fst $1)) (hsSymbol (snd $1)) }

consym :: { HsName }
      : CONSYM                { UnQual (hsSymbol $1) }

qvarsym :: { HsName }
      : varsym                { $1 }
      | qvarsym1              { $1 }

qvarsymm :: { HsName }
      : varsymm               { $1 }
      | qvarsym1              { $1 }

varsym :: { HsName }
      : VARSYM                { UnQual (hsSymbol $1) }
      | '-'                   { minus_name }
      | '!'                   { pling_name }
      | '??'                  { UnQual (hsSymbol "??") }
      | '*!'                  { UnQual (hsSymbol "*!") }
      | '*'                   { star_name }
      | '#'                   { hash_name }
      | '.'                   { dot_name }

varsymm :: { HsName } -- varsym not including '-'
      : VARSYM                { UnQual (hsSymbol $1) }
      | '!'                   { pling_name }
      | '*'                   { star_name }
      | '#'                   { hash_name }
      | '.'                   { dot_name }

qvarsym1 :: { HsName }
      : QVARSYM               { Qual (Module (fst $1)) (hsSymbol (snd $1)) }

literal :: { HsExp }
      : INT                   { HsLit (HsInt (readInteger $1)) }
      | UINT                  { HsLit (HsIntPrim (readInteger $1)) }
      | CHAR                  { HsLit (HsChar $1) }
      | UCHAR                 { HsLit (HsCharPrim $1) }
      | RATIONAL              { HsLit (HsFrac (readRational $1)) }
      | STRING                { HsLit (HsString $1) }
      | USTRING               { HsLit (HsStringPrim $1) }

 srcloc :: { SrcLoc } :       {% getSrcLoc }

-- -----------------------------------------------------------------------------
-- Layout

close :: { () }
      : vccurly               { () } -- context popped in lexer.
      | error                 {% popContext }

layout_on  :: { () }  :       {% getSrcLoc `thenP` \sl ->
                                 pushCurrentContext  }

--                                 pushCurrentContext (Layout (srcLocColumn sl)) }

-- -----------------------------------------------------------------------------
-- Miscellaneous (mostly renamings)

modid :: { Module }
      : CONID                 { Module $1 }
      | QCONID                { Module (fst $1 ++ "." ++ snd $1) }

tyconorcls :: { HsName }
      : conid                 { $1 }

tycon :: { HsName }
      : conid                 { $1 }

qtyconorcls :: { HsName }
      : qconid                { $1 }

qtycls :: { HsName }
      : qconid                { $1 }

tyvar :: { HsName }
      : varid                 { $1 }

-- -----------------------------------------------------------------------------

{
happyError = parseError "Parse error"
hsSymbol x = HsIdent x
readInteger x = fromIntegral x
readRational x = x
}
