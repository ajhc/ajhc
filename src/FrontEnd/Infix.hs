{-------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)

        Module:                 Infix

        Description:            Patches the abstract syntax description with
                                the infix precedence and associativity rules
                                for identifiers in the module.

                                The main tasks implemented by this module are:

        Primary Authors:        Lindsay Powles

        Notes:                  See the file License for license information

-------------------------------------------------------------------------------}

module FrontEnd.Infix (buildFixityMap, infixHsModule, FixityMap,size, infixStatement, restrictFixityMap) where

import Data.Binary
import Data.Monoid
import qualified Data.Map as Map

import FrontEnd.HsSyn
import FrontEnd.Lex.ParseMonad
import FrontEnd.Lex.Post
import FrontEnd.SrcLoc
import FrontEnd.Syn.Traverse
import Name.Name
import Name.Names
import Options
import Support.MapBinaryInstance
import Util.HasSize
import qualified FlagOpts as FO
import qualified FrontEnd.Lex.Fixity as F

type FixityInfo = (Int, HsAssoc)
type SymbolMap = Map.Map Name FixityInfo

newtype FixityMap = FixityMap SymbolMap
    deriving(Monoid,HasSize)

instance Binary FixityMap where
    put (FixityMap ts) = putMap ts
    get = fmap FixityMap getMap

restrictFixityMap :: (Name -> Bool) -> FixityMap -> FixityMap
restrictFixityMap f (FixityMap fm) = FixityMap (Map.filterWithKey (\k _ -> f k) fm)

----------------------------------------------------------------------------

 -- Some constants:

syn_err_msg :: String
syn_err_msg = "Syntax error in input, run through a compiler to check.\n"

syn_err_bad_oparg op exp =    syn_err_msg ++ "\tERROR: cannot apply " ++ show op
                           ++ " to the expression: " ++ show exp

syn_err_precedence op exp =    syn_err_msg ++ "\tERROR: the precedence of " ++ show op
                            ++ " is incompatible with the precendence of it's argument: " ++ show exp

defaultFixity :: (Int, HsAssoc)     -- Fixity assigned to operators without explict infix declarations.
defaultFixity = (9, HsAssocLeft)

terminalFixity :: (Int, HsAssoc)    -- Fixity given to variables, etc. Used to terminate descent.
terminalFixity = (10, HsAssocLeft)

infixHsModule :: FixityMap -> HsModule -> HsModule
infixHsModule (FixityMap ism) m =  ans where
    ans = if fopts' (hsModuleOpt m) FO.NewParser
        then domod m
        else hsModuleDecls_u (map (processDecl ism)) m
    (expShuntSpec,pexpShuntSpec) = (expShuntSpec,pexpShuntSpec) where
        pexpShuntSpec = expShuntSpec {
            F.operator = paren_operator, F.trailingOps }
        expShuntSpec = F.shuntSpec {
            F.lookupToken,
            F.application ,
            F.operator,
            F.lookupUnary }
        lookupToken (HsBackTick bt) = backtick bt
        lookupToken (HsAsPat x v) = mr (HsAsPat x) v
        lookupToken (HsLocatedExp (Located sl v)) = mr (HsLocatedExp . Located sl) v
        lookupToken t = return (Left t)
        lookupUnary t = return Nothing
        application e1 e2 = return $ HsApp e1 (hsParen e2)
        operator (HsBackTick t) as = operator t as
        operator (HsVar v) [e] | v == v_sub = return $ HsNegApp (hsParen e)
        operator t as = return $ foldl HsApp t (map hsParen as)
        paren_operator (HsBackTick t) as = paren_operator t as
        paren_operator (HsVar v) [e] | v == v_sub = return $ HsNegApp (hsParen e)
        paren_operator t [e] = return $ HsRightSection (hsParen e) t
        paren_operator t as = operator t as
        trailingOps e (HsBackTick t) = trailingOps e t
        trailingOps e t = return $ HsLeftSection t (hsParen e)
        backtick bt = f bt where
            f (HsVar v) = g v
            f ~(HsCon v) = g v
            --f (HsAsPat _ v) = backtick v
            g v = return $ case Map.lookup v ism of
                Just (n,HsAssocLeft) -> Right (F.L,n)
                Just (n,HsAssocRight) -> Right (F.R,n)
                Just (n,HsAssocNone) -> Right (F.N,n)
                Just (n,HsAssocPrefix) -> Right (F.Prefix,n)
                Just (n,HsAssocPrefixy) -> Right (F.Prefixy,n)
                Nothing -> Right (F.L,9)
        mr x v = do
            n <- lookupToken v
            case n of
                Left v -> return $ Left $ x v
                Right {} -> return n
    patShuntSpec =  F.shuntSpec {
            F.lookupToken,
            F.application,
            F.operator,
            F.lookupUnary } where
        lookupToken (HsPatBackTick bt) = backtick bt
        lookupToken t = return (Left t)
        lookupUnary t = return Nothing
        application (HsPApp t es) y = return $ HsPApp t (es ++ [y])
        application x y = parseErrorK $ "weird appliaction: " ++ show (x,y)
        operator (HsPatBackTick t) as = operator t as
        operator (HsPVar v) [e] | v == u_Bang = do sl <- getSrcSpan; return $ HsPBangPat (Located sl e)
        operator (HsPVar v) [e] | v == u_Twiddle = do sl <- getSrcSpan; return $ HsPIrrPat (Located sl e)
        operator (HsPVar v) [HsPVar ap, e] | v == u_At = do sl <- getSrcSpan; return $ HsPAsPat ap e
        operator (HsPVar v) [e] | v == v_sub = return $ HsPNeg e
        operator (HsPApp t xs) y = return $ HsPApp t (xs ++ y)
        backtick bt = f bt where
            f (HsPVar v) | v == u_Bang = return (Right (F.Prefix,11))
            f (HsPVar v) | v == u_Twiddle = return (Right (F.Prefix,11))
            f (HsPVar v) | v == u_At = return (Right (F.R,11))
            f (HsPVar v) = g v
            f (HsPApp v []) = g v
            g v = return $ case Map.lookup v ism of
                Just (n,HsAssocLeft) -> Right (F.L,n)
                Just (n,HsAssocRight) -> Right (F.R,n)
                Just (n,HsAssocNone) -> Right (F.N,n)
                Just (n,HsAssocPrefix) -> Right (F.Prefix,n)
                Just (n,HsAssocPrefixy) -> Right (F.Prefixy,n)
                Nothing -> Right (F.L,9)
    domod m = case runP (traverseHsOps ops m) (hsModuleOpt m) of
        (ws,~(Just v)) -> if null ws then v else error $ unlines (map show ws)
    ops = (hsOpsDefault ops) { opHsExp, opHsPat } where
        opHsExp (HsParen (HsWords es)) = F.shunt pexpShuntSpec es >>= applyHsOps ops
        opHsExp (HsWords es) = F.shunt expShuntSpec es >>= applyHsOps ops
        opHsExp (HsBackTick t) = parseErrorK "unexpected binary operator."
--        opHsExp (HsParen (HsBackTick bt)) = parseErrorK "parens around backtick"
        opHsExp e = traverseHsOps ops e
--        opHsPat (HsPatExp e) = do
 --           e <- opHsExp e
  --          checkPattern e
        opHsPat (HsPatWords ws) = F.shunt patShuntSpec ws >>= applyHsOps ops
        opHsPat p = traverseHsOps ops p

infixStatement :: FixityMap -> HsStmt -> HsStmt
infixStatement (FixityMap ism) m = processStmt ism m

--infixer :: [HsDecl] -> TidyModule -> TidyModule
--infixer infixRules tidyMod =
--    tidyMod { tidyClassDecls = process tidyClassDecls,
--              tidyInstDecls = process tidyInstDecls,
--              tidyFunBinds = process tidyFunBinds,
--              tidyPatBinds = process tidyPatBinds }
--    where
--        process field = map (processDecl infixMap) (field tidyMod)
--        infixMap = buildSMap infixRules

----------------------------------------------------------------------------

  --  Functions for building and searching the map of operators and their
  -- associated associativity and binding power.

buildFixityMap :: [HsDecl] -> FixityMap
buildFixityMap ds = FixityMap (Map.fromList $ concatMap f ds)  where
        f (HsInfixDecl _ assoc strength names) = zip (map make_key names) $ repeat (strength,assoc)
        f _ = []
        make_key = fromValishHsName
        --make_key a_name = case a_name of
        --    (Qual a_module name)   -> (a_module, name)
        --    (UnQual name)          -> (unqualModule, name)

--buildSMap infixRules =
--    foldl myAddToFM emptyFM $ concat $ map formatDecl infixRules
--    where
--        formatDecl (HsInfixDecl _ assoc strength names) = zip (map make_key names) $ circList (strength,assoc)
--        formatDecl _ = []
--        circList (str,assc) = (str,assc) : circList (str,assc)
--        myAddToFM fm (k,e) = addToFM fm k e
--        make_key a_name = case a_name of
--            (Qual a_module name)   -> (a_module, name)
--            (UnQual name)          -> (unqualModule, name)

lookupSM infixMap  exp = case exp of
    HsAsPat _ e -> lookupSM infixMap e
    HsVar qname    -> Map.findWithDefault defaultFixity (toName Val qname) infixMap
    HsCon qname    -> Map.findWithDefault defaultFixity (toName DataConstructor qname) infixMap
    _           -> error $ "Operator (" ++ show exp ++ ") is invalid."

--lookupSM infixMap  exp = case exp of
--    HsAsPat _ e -> lookupSM infixMap e
--    HsVar qname    -> case qname of
--                    Qual a_module name -> lookupDftFM infixMap defaultFixity (a_module, name)
--                    UnQual name        -> lookupDftFM infixMap defaultFixity (unqualModule, name)
--    HsCon qname  -> case qname of
--                    Qual a_module name -> lookupDftFM infixMap defaultFixity (a_module, name)
--                    UnQual name        -> lookupDftFM infixMap defaultFixity (unqualModule, name)
--    _           -> error $ "Operator (" ++ show exp ++ ") is invalid."

-----------------------------------------------------------------------------

  --  Functions used to sift through the syntax to find expressions to
  -- operate on.

processDecl :: SymbolMap -> HsDecl -> HsDecl
processDecl infixMap decl = case decl of
    HsClassDecl    srcloc qualtype decls   -> HsClassDecl srcloc qualtype $ proc_decls decls
    HsInstDecl     srcloc qualtype decls   -> HsInstDecl srcloc qualtype $ proc_decls decls
    HsFunBind      matches                 -> HsFunBind $ map (processMatch infixMap) matches
    HsPatBind      srcloc pat rhs decls    -> HsPatBind srcloc (procPat infixMap pat) (processRhs infixMap rhs) $ proc_decls decls
    HsPragmaRules rs -> HsPragmaRules $ map proc_rule rs
    _                                       -> decl
    where
        proc_decls decls = map (processDecl infixMap) decls
        proc_rule prules@HsRule { hsRuleLeftExpr = e1, hsRuleRightExpr = e2} =
             prules { hsRuleLeftExpr = fst $ processExp infixMap e1, hsRuleRightExpr = fst $ processExp infixMap e2 }

processMatch :: SymbolMap -> HsMatch -> HsMatch
processMatch infixMap (HsMatch srcloc qname pats rhs decls) =
    HsMatch srcloc qname (map (procPat infixMap) pats) new_rhs new_decls
    where
        new_rhs = processRhs infixMap rhs
        new_decls = map (processDecl infixMap) decls

processRhs :: SymbolMap -> HsRhs -> HsRhs
processRhs infixMap rhs = case rhs of
    HsUnGuardedRhs exp     -> HsUnGuardedRhs $ fst $ processExp infixMap exp
    HsGuardedRhss  rhss    -> HsGuardedRhss $ map (processGRhs infixMap) rhss

processGRhs :: SymbolMap -> HsGuardedRhs -> HsGuardedRhs
processGRhs infixMap (HsGuardedRhs srcloc e1 e2) = HsGuardedRhs srcloc new_e1 new_e2
    where
        new_e1 = fst $ processExp infixMap e1
        new_e2 = fst $ processExp infixMap e2

processAlt :: SymbolMap -> HsAlt -> HsAlt
processAlt infixMap (HsAlt srcloc pat g_alts decls) = HsAlt srcloc (procPat infixMap pat) new_g_alts new_decls
    where
        new_g_alts = processGAlts infixMap g_alts
        new_decls = map (processDecl infixMap) decls

processGAlts :: SymbolMap -> HsRhs -> HsRhs
processGAlts infixMap g_alts = case g_alts of
    HsUnGuardedRhs exp     -> HsUnGuardedRhs $ fst $ processExp infixMap exp
    HsGuardedRhss galts    -> HsGuardedRhss $ map (processGAlt infixMap) galts

processGAlt :: SymbolMap -> HsGuardedRhs -> HsGuardedRhs
processGAlt infixMap (HsGuardedRhs srcloc e1 e2) = HsGuardedRhs srcloc new_e1 new_e2
    where
        new_e1 = fst $ processExp infixMap e1
        new_e2 = fst $ processExp infixMap e2

processStmt :: SymbolMap -> HsStmt -> HsStmt
processStmt infixMap stmt = case stmt of
    HsGenerator srcloc pat exp     -> HsGenerator srcloc (procPat infixMap pat) $ fst $ processExp infixMap exp
    HsQualifier exp                -> HsQualifier $ fst $ processExp infixMap exp
    HsLetStmt decls                -> HsLetStmt $ map (processDecl infixMap) decls
 -- _                           -> error "Bad HsStmt data passed to processStmt."

processFUpdt :: SymbolMap -> HsFieldUpdate -> HsFieldUpdate
processFUpdt infixMap (HsField qname exp) = HsField qname new_exp
    where
        new_exp = fst $ processExp infixMap exp

procPat sm p = fst $ processPat sm p
processPat :: SymbolMap -> HsPat -> (HsPat, FixityInfo)
processPat infixMap exp = case exp of
    HsPInfixApp l op r  -> case (compare l_power op_power) of
            GT -> (HsPInfixApp new_l op new_r, op_fixity)
            EQ -> case op_assoc of
                HsAssocNone    -> error_precedence op new_l
                HsAssocRight   -> case l_assoc of
                    HsAssocRight   -> case new_l of
                        HsPInfixApp l' op' r' -> (HsPInfixApp l' op' (process_r' r'), l_fixity)
                        _                     -> error_syntax op new_l
                    _               -> error_precedence op new_l
                HsAssocLeft    -> case l_assoc of
                    HsAssocLeft    -> (HsPInfixApp new_l op new_r, op_fixity)
                    _               -> error_precedence op new_l
                _ -> error "processPat.infix.a"
            LT -> case new_l of
                HsPInfixApp l' op' r' -> (HsPInfixApp l' op' (process_r' r'), l_fixity)
                _                     -> error_syntax op new_l
        where
            (new_l, l_fixity) = processPat infixMap l
            l_power = fst l_fixity
            l_assoc = snd l_fixity
            op_fixity = Map.findWithDefault defaultFixity  (toName DataConstructor op) infixMap
            op_power = fst op_fixity
            op_assoc = snd op_fixity
            new_r = processExp' r
            process_r' r' = processExp' $ HsPInfixApp r' op r
            error_precedence err_op err_lower = error $ syn_err_precedence err_op err_lower
            error_syntax err_op err_lower = error $ syn_err_bad_oparg err_op err_lower
    x@HsPVar {} -> (x,terminalFixity)
    x@HsPLit {} -> (x,terminalFixity)
    x@HsPWildCard  -> (x,terminalFixity)
    HsPNeg p ->    tf $ HsPNeg (pp p)
    HsPIrrPat p -> tf $ HsPIrrPat (fmap pp p)
    HsPBangPat p -> tf $ HsPBangPat (fmap pp p)
    HsPApp n xs -> tf $ HsPApp n (map pp xs)
    HsPTuple xs -> tf $ HsPTuple (map pp xs)
    HsPUnboxedTuple xs -> tf $ HsPUnboxedTuple (map pp xs)
    HsPList xs ->  tf $ HsPList (map pp xs)
    HsPParen xs -> tf $ HsPParen (pp xs)
    HsPRec n xs -> tf $ HsPRec n [ HsField n (pp p) | HsField n p <- xs ]
    HsPAsPat n p -> tf $ HsPAsPat n (pp p)
    ~(HsPTypeSig sl p qt) -> tf $ HsPTypeSig sl (pp p) qt
    where
        processExp' = fst . (processPat infixMap)
        pp = fst . (processPat infixMap)
        tf x = (x,terminalFixity)

-----------------------------------------------------------------------------

    {- processExp():   Where the syntax tree reshaping actually takes
                     place. Assumes the parser that created the syntax
                     assumed the same binding power and left associativity
                     for all operators. Operators are assumed to be only
                     those that are excepted under the Haskell 98 report
                     and sections are also parsed according to this report
                     aswell (NOT according to how current compilers handle
                     sections!). -}

processExp :: SymbolMap -> HsExp -> (HsExp, FixityInfo)
processExp infixMap exp = case exp of
    HsInfixApp l op r  ->
              case (compare l_power op_power) of
                    GT -> (HsInfixApp new_l op new_r, op_fixity)
                    EQ -> case op_assoc of
                        HsAssocNone    -> error_precedence op new_l
                        HsAssocRight   -> case l_assoc of
                            HsAssocRight   -> case new_l of
                                HsInfixApp l' op' r' -> (HsInfixApp l' op' (process_r' r'), l_fixity)
                                _                     -> error_syntax op new_l
                            _               -> error_precedence op new_l
                        HsAssocLeft    -> case l_assoc of
                            HsAssocLeft    -> (HsInfixApp new_l op new_r, op_fixity)
                            _               -> error_precedence op new_l
                        _ -> error "processExp.infix.b"
                    LT -> case new_l of
                        HsInfixApp l' op' r' -> (HsInfixApp l' op' (process_r' r'), l_fixity)
                        _                     -> error_syntax op new_l
               where
                    (new_l, l_fixity) = processExp infixMap l
                    l_power = fst l_fixity
                    l_assoc = snd l_fixity
                    op_fixity = lookupSM infixMap op
                    op_power = fst op_fixity
                    op_assoc = snd op_fixity
                    new_r = processExp' r
                    process_r' r' = processExp' $ HsInfixApp r' op r
                    error_precedence err_op err_lower = error $ syn_err_precedence err_op err_lower
                    error_syntax err_op err_lower = error $ syn_err_bad_oparg err_op err_lower
    HsApp e1 e2        -> (HsApp (processExp' e1) (processExp' e2), terminalFixity)
    HsNegApp e1        -> (HsNegApp (processExp' e1), terminalFixity)
    HsLet decls e1     -> (HsLet (map (processDecl infixMap) decls) (processExp' e1), terminalFixity)
    HsIf e1 e2 e3      -> (HsIf (processExp' e1) (processExp' e2) (processExp' e3), terminalFixity)
    HsCase e1 alts     -> (HsCase (processExp' e1) (map (processAlt infixMap) alts), terminalFixity)
    HsDo stmts         -> (HsDo (map (processStmt infixMap) stmts), terminalFixity)
    HsTuple exps       -> (HsTuple (map processExp' exps), terminalFixity)
    HsUnboxedTuple exps -> (HsUnboxedTuple (map processExp' exps), terminalFixity)
    HsList exps        -> (HsList (map processExp' exps), terminalFixity)
    HsParen e1         -> (HsParen (processExp' e1), terminalFixity)
    HsEnumFrom e1      -> (HsEnumFrom (processExp' e1), terminalFixity)
    HsEnumFromTo e1 e2 -> (HsEnumFromTo (processExp' e1) (processExp' e2), terminalFixity)
    HsListComp e1 stmts    ->
                           (HsListComp (processExp' e1) (map (processStmt infixMap) stmts), terminalFixity)
    HsAsPat name e1        -> (HsAsPat name (processExp' e1), terminalFixity)
    HsIrrPat e1            -> (HsIrrPat (fmap processExp' e1), terminalFixity)
    HsBangPat e1            -> (HsBangPat (fmap processExp' e1), terminalFixity)
    HsLeftSection e1 e2    -> (HsLeftSection e1 (processExp' e2), terminalFixity)
    HsRightSection e1 e2       -> (HsRightSection (processExp' e1) e2, terminalFixity)
    HsLambda srcloc pats e1    -> (HsLambda srcloc (map (procPat infixMap) pats) (processExp' e1), terminalFixity)
    HsRecConstr qname f_updts  -> (HsRecConstr qname (map (processFUpdt infixMap) f_updts), terminalFixity)
    HsEnumFromThen e1 e2       -> (HsEnumFromThen (processExp' e1) (processExp' e2), terminalFixity)
    HsRecUpdate e1 f_updts     ->
                        (HsRecUpdate (processExp' e1) (map (processFUpdt infixMap) f_updts), terminalFixity)
    HsEnumFromThenTo e1 e2 e3  ->
                        (HsEnumFromThenTo (processExp' e1) (processExp' e2) (processExp' e3), terminalFixity)
    HsExpTypeSig srcloc e1 qtype   -> (HsExpTypeSig srcloc (processExp' e1) qtype, terminalFixity)
    _                   -> (exp, terminalFixity)
    where
        processExp' = fst . (processExp infixMap)

------------------------------------------------------------------------------
