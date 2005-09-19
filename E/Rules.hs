module E.Rules(
    Rules,
    Rule(ruleHead,ruleBinds,ruleArgs,ruleBody,ruleName),
    ruleFreeVars,
    ruleAllFreeVars,
    applyRule'',
    ruleFreeVars',
    fromRules,
    emptyRule,
    printRule,
    printRules,
    applyRule,
    mapBodies,
    applyRule'
    )where

import Data.Monoid
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom(toAtom,fromAtom,Atom)
import Binary
import E.E
import E.Eval
import E.Pretty
import E.Subst
import FreeVars
import GenUtil
import HasSize
import MapBinaryInstance()
import Name
import Stats




data Rule = Rule {
    ruleHead :: TVr,
--    ruleFvs :: Set.Set Int,
    ruleBinds :: [TVr],
    ruleArgs :: [E],
    ruleBody :: E,
    ruleName :: Atom
    }
 {-! derive: GhcBinary !-}


emptyRule :: Rule
emptyRule = Rule {
    ruleHead = error "ruleHead undefined",
--    ruleFvs = error "ruleFvs undefined",
    ruleArgs = [],
    ruleBinds = [],
    ruleBody = error "ruleBody undefined",
    ruleName = error "ruleName undefined"
    }

newtype Rules = Rules (Map.Map TVr [Rule])
    deriving(HasSize,Binary)

mapBodies :: (E ->  E) -> Rules ->  Rules
mapBodies g (Rules mp) = Rules $ Map.map (map f) mp where
    f rule = rule { ruleBody = g (ruleBody rule) }
    --    return rule { ruleBody = b }


ruleAllFreeVars (Rules r) = freeVars (concatMap (map ruleBody) (Map.elems r))

ruleFreeVars ::  Rules -> TVr -> Set.Set Int
ruleFreeVars (Rules r) tvr = case Map.lookup tvr r of
    Nothing -> mempty
    --Just rs -> mconcat (map ruleFvs rs) -- (freeVars (map ruleBody rs) Set.\\ freeVars (map ruleArgs rs))
    Just rs -> (freeVars (map ruleBody rs) Set.\\ freeVars (map ruleArgs rs))
ruleFreeVars' ::  Rules -> Int -> Set.Set Int
ruleFreeVars' (Rules r) tvr = case Map.lookup (tVr tvr undefined) r of
    Nothing -> mempty
    --Just rs -> mconcat (map ruleFvs rs) -- (freeVars (map ruleBody rs) Set.\\ freeVars (map ruleArgs rs))
    Just rs -> (freeVars (map ruleBody rs) Set.\\ freeVars (map ruleArgs rs))

printRule rule = do
    putErrLn $ fromAtom (ruleName rule)
    putErr $ "    " ++ render (ePretty (foldl EAp (EVar $ ruleHead rule) (ruleArgs rule)))
    putErrLn $ " -> " ++ render (ePretty (ruleBody rule))

printRules (Rules rules) = mapM_ printRule (concat $ Map.elems rules)

combineRules as bs = map head $ sortGroupUnder ruleName (as ++ bs)

instance Monoid Rules where
    mempty = Rules mempty
    mappend (Rules x) (Rules y) = Rules $ Map.unionWith (combineRules) x y


fromRules :: [Rule] -> Rules
fromRules rs = Rules $ Map.map snds $ Map.fromList $ sortGroupUnderF fst [ (ruleHead r,f r) | r <- rs ] where
    f rule = rule
    --f rule = rule { ruleFvs = fvs rule } where
    --fvs rule = (freeVars $ ruleBody rule) Set.\\ freeVars (ruleArgs rule)


applyRule stats rules (EVar tvr) xs = do
    z <- applyRule' stats rules tvr xs
    case z of
        Just (x,xs) -> return $ foldl EAp x xs
        Nothing -> return $ foldl EAp (EVar tvr) xs
applyRule _stats _rules _ _xs = fail "Can't apply rule"

preludeError = nameValue "Prelude" "error"
ruleError = toAtom "Rule.error/EError"

applyRule' stats _ (TVr { tvrIdent = n }) (ty:s:rs) | n == preludeError, Just s' <- toString s  = do
        tick stats ruleError
        return $ Just ((EError ("Prelude.error: " ++ s') ty),rs)
applyRule' stats (Rules rules) tvr xs = ans where
    ans = case Map.lookup tvr rules of
            Just rs -> f rs
            _ -> return Nothing
    f [] = return Nothing
    f (r:_) | nArgs <= length xs, Just ss <- sequence (zipWith unify (ruleArgs r) xs) = ans ss where
        nArgs = length (ruleArgs r)
        ans ss = do
            tick stats (ruleName r)
            let b = substMap (IM.fromList [ (i,x) | ~(~(EVar (TVr { tvrIdent = i })),x) <- concat ss ]) (ruleBody r)
            return $ Just (b,(drop nArgs xs))
    f (_:rs) = f rs

applyRule'' _ (TVr { tvrIdent = n }) (ty:s:rs) | n == preludeError, Just s' <- toString s  = do
        mtick ruleError
        return $ Just ((EError ("Prelude.error: " ++ s') ty),rs)
applyRule'' (Rules rules) tvr xs = ans where
    ans = case Map.lookup tvr rules of
            Just rs -> f rs
            _ -> return Nothing
    f [] = return Nothing
    f (r:_) | nArgs <= length xs, Just ss <- sequence (zipWith unify (ruleArgs r) xs) = ans ss where
        nArgs = length (ruleArgs r)
        ans ss = do
            mtick (ruleName r)
            let b = substMap (IM.fromList [ (i,x) | ~(~(EVar (TVr { tvrIdent = i })),x) <- concat ss ]) (ruleBody r)
            return $ Just (b,(drop nArgs xs))
    f (_:rs) = f rs



