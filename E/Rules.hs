module E.Rules(
    Rules,
    Rule(ruleHead,ruleBinds,ruleArgs,ruleBody,ruleName),
    ruleFreeVars,
    ruleAllFreeVars,
    ruleFreeVars',
    fromRules,
    emptyRule,
    printRule,
    printRules,
    mapBodies,
    hasBuiltinRule,
    getARules,
    arules,
    ARules,
    applyRules,
    builtinRule
    )where

import Data.Typeable
import Data.Monoid
import Monad(liftM)
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
import Util.HasSize
import MapBinaryInstance()
import Name
import Stats




data Rule = Rule {
    ruleHead :: TVr,
--    ruleFvs :: Set.Set Int,
    ruleBinds :: [TVr],
    ruleArgs :: [E],
    ruleNArgs :: {-# UNPACK #-} !Int,
    ruleBody :: E,
    ruleName :: Atom
    }
 {-! derive: GhcBinary !-}


instance Show Rule where
    showsPrec _ r = showString (fromAtom $ ruleName r)

emptyRule :: Rule
emptyRule = Rule {
    ruleHead = error "ruleHead undefined",
--    ruleFvs = error "ruleFvs undefined",
    ruleArgs = [],
    ruleNArgs = 0,
    ruleBinds = [],
    ruleBody = error "ruleBody undefined",
    ruleName = error "ruleName undefined"
    }

newtype Rules = Rules (Map.Map Id [Rule])
    deriving(HasSize)


instance Binary Rules where
    put_ h (Rules mp) = putNList h (concat $ Map.elems mp)
    get h = do
        rs <- getNList h
        return $ fromRules rs

mapBodies :: Monad m => (E -> m E) -> Rules -> m Rules
mapBodies g (Rules mp) = do
    let f rule = do
            b <- g (ruleBody rule)
            return rule { ruleBody = b }
    mp' <- sequence [ do rs' <- mapM f rs; return (k,rs') | (k,rs) <- Map.toAscList mp ]
    return $ Rules $ Map.fromAscList mp'


ruleAllFreeVars (Rules r) = freeVars (concatMap (map ruleBody) (Map.elems r))

ruleFreeVars ::  Rules -> TVr -> Set.Set Int
ruleFreeVars (Rules r) tvr = case Map.lookup (tvrIdent tvr) r of
    Nothing -> mempty
    --Just rs -> mconcat (map ruleFvs rs) -- (freeVars (map ruleBody rs) Set.\\ freeVars (map ruleArgs rs))
    Just rs -> (freeVars (map ruleBody rs) Set.\\ freeVars (map ruleArgs rs))

ruleFreeVars' ::  Rules -> Id -> Set.Set Int
ruleFreeVars' (Rules r) tvr = case Map.lookup tvr r of
    Nothing -> mempty
    --Just rs -> mconcat (map ruleFvs rs) -- (freeVars (map ruleBody rs) Set.\\ freeVars (map ruleArgs rs))
    Just rs -> (freeVars (map ruleBody rs) Set.\\ freeVars (map ruleArgs rs))


instance FreeVars Rule b => FreeVars ARules b where
    freeVars (ARules rs) = freeVars rs

instance FreeVars Rule (Set.Set TVr) where
    freeVars rule = freeVars (ruleBody rule) Set.\\ freeVars (ruleArgs rule)

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
fromRules rs = Rules $ Map.map snds $ Map.fromList $ sortGroupUnderF fst [ (tvrIdent $ ruleHead r,r) | r <- rs ]

getARules :: Monad m => Rules -> Id -> m ARules
getARules (Rules mp) tvr = liftM arules (Map.lookup tvr mp)


-- | invarients for ARules
-- sorted by number of arguments rule takes
-- all hidden rule fields filled in

newtype ARules = ARules [Rule]
    deriving(Show,Typeable)

arules xs = ARules (sortUnder ruleNArgs (map f xs)) where
    f rule = rule { ruleNArgs = length  (ruleArgs rule) }

instance Monoid ARules where
    mempty = ARules []
    mappend (ARules a) (ARules b) = ARules (sortUnder ruleNArgs (a ++ b))


-- applyRules :: ARules -> [E] -> IO (Maybe (E,[E]))
applyRules (ARules rs) xs = f rs where
    lxs = length xs
    f [] = return Nothing
    f (r:_) | ruleNArgs r > lxs = return Nothing
    f (r:_) | Just ss <- sequence (zipWith unify (ruleArgs r) xs) = ans ss where
        ans ss = do
            mtick (ruleName r)
            let b = substMap (IM.fromList [ (i,x) | ~(~(EVar (TVr { tvrIdent = i })),x) <- concat ss ]) (ruleBody r)
            return $ Just (b,drop (ruleNArgs r) xs)
    f (_:rs) = f rs


preludeError = nameValue "Prelude" "error"
ruleError = toAtom "Rule.error/EError"

hasBuiltinRule TVr { tvrIdent = n } = n `Set.member` Set.fromList [preludeError]
builtinRule TVr { tvrIdent = n } (ty:s:rs)
    | n == preludeError, Just s' <- toString s  = do
        mtick ruleError
        return $ Just ((EError ("Prelude.error: " ++ s') ty),rs)
builtinRule _ _ = return Nothing

