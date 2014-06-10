module FrontEnd.Infix (
    buildFixityMap, infixHsModule, FixityMap,size,
    infixStatement, restrictFixityMap, dumpFixityMap) where

import Data.Binary
import Data.Monoid
import qualified Data.Map as Map

import FrontEnd.HsSyn
import FrontEnd.Lex.ParseMonad
import FrontEnd.Syn.Traverse
import FrontEnd.Warning
import Name.Names
import Support.MapBinaryInstance
import Util.HasSize
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

dumpFixityMap :: FixityMap -> IO ()
dumpFixityMap (FixityMap ts) = do
    mapM_ print (Map.toList ts)

infixHsModule :: FixityMap -> HsModule -> HsModule
infixHsModule (FixityMap ism) m =  domod m where
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
        application x y = do
            parseErrorK $ "weird application: " ++ show (x,y)
            return HsPWildCard
        operator ~(HsPatBackTick t) as = f t as where
            f (HsPVar v) [e] | v == u_Bang = do sl <- getSrcSpan; return $ HsPBangPat (Located sl e)
            f (HsPVar v) [e] | v == u_Twiddle = do sl <- getSrcSpan; return $ HsPIrrPat (Located sl e)
            f (HsPVar v) [HsPVar ap, e] | v == u_At = do sl <- getSrcSpan; return $ HsPAsPat ap e
            f (HsPVar v) [HsPWildCard, e] | v == u_At = do return e
            f (HsPVar v) [e] | originalUnqualifiedName v == vu_sub = return $ HsPNeg e
            f (HsPApp t xs) y = return $ HsPApp t (xs ++ y)
            f x@(HsPVar v) y = do
                parseErrorK $ "weird operator: " ++ show (v,originalUnqualifiedName v,x,y)
                return HsPWildCard
            f x y = do
                parseErrorK $ "weird operator: " ++ show (x,y)
                return HsPWildCard

        backtick bt = f bt where
            f (HsPVar v) | v == u_Bang = return (Right (F.Prefix,11))
            f (HsPVar v) | v == u_Twiddle = return (Right (F.Prefix,11))
            f (HsPVar v) | v == u_At = return (Right (F.R,12))
            f (HsPVar v) = g v
            f (HsPApp v []) = g v
            f z = parseErrorK $ "infix.f: " ++ show z
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
        opHsExp e@(HsApp HsTuple {} _) = do
                addWarn UnexpectedType $ "Attempt to apply a tuple like a function in expression"
                traverseHsOps ops e
        opHsExp e@(HsApp HsUnboxedTuple {} _) = do
                addWarn UnexpectedType $ "Attempt to apply an unboxed tuple like a function in expression"
                traverseHsOps ops e
        opHsExp e@(HsApp HsList {} _) = do
                addWarn UnexpectedType $ "Attempt to apply literal list like a function in expression"
                traverseHsOps ops e
        opHsExp op@HsApp {} | (ce,ps) <- fromHsApp op = do
            let cont = do
                    ps <- mapM opHsExp (ce:ps)
                    return $ foldl1 HsApp ps
                err s = addWarn UnexpectedType s >> cont
            case ce of
                HsCon c | Just (isBoxed,level,n) <- fromName_Tuple c, level == termLevel -> case length ps of
                    lps | lps > n -> err "Attempt to apply a tuple like a function in expression"
                        | lps < n -> cont
                        | otherwise -> opHsExp $ if isBoxed then HsTuple ps else HsUnboxedTuple ps
                HsList {} -> err "Attempt to apply literal list like a function in expression"
                HsTuple {} -> err "Attempt to apply a tuple like a function in expression"
                HsUnboxedTuple {} -> err "Attempt to apply an unboxed tuple like a function in expression"
                _ -> cont
        opHsExp (HsBackTick t) = parseErrorK "unexpected binary operator."
        opHsExp e = traverseHsOps ops e

        opHsPat (HsPatWords ws) = F.shunt patShuntSpec ws >>= opHsPat
        opHsPat op@(HsPApp n ps) | Just (isBoxed,level,n) <- fromName_Tuple n, level == termLevel = case length ps of
            lps | lps > n -> do
                addWarn UnexpectedType $ "Applying tuple to too many arguments in pattern"
                traverseHsOps ops op
                | lps < n -> do
                addWarn UnexpectedType $ "Applying tuple constructor to not enough arguments in pattern"
                traverseHsOps ops op
                | otherwise -> opHsPat $ if isBoxed then HsPTuple ps else HsPUnboxedTuple ps
        opHsPat p = traverseHsOps ops p

fromHsApp t = f t [] where
    f (HsApp a b) rs = f a (b:rs)
    f t rs = (t,rs)

buildFixityMap :: [HsDecl] -> FixityMap
buildFixityMap ds = FixityMap (Map.fromList $ concatMap f ds)  where
        f (HsInfixDecl _ assoc strength names) = zip (map make_key names) $ repeat (strength,assoc)
        f _ = []
        make_key = fromValishHsName

-- TODO: interactive
infixStatement :: FixityMap -> HsStmt -> HsStmt
infixStatement (FixityMap ism) m = m
--infixStatement (FixityMap ism) m = processStmt ism m
