module E.Rules(
    ARules,
    Rule(ruleHead,ruleBinds,ruleArgs,ruleBody,ruleUniq,ruleName),
    Rules,
    applyRules,
    arules,
    bindingFreeVars,
    builtinRule,
    dropArguments,
    emptyRule,
    fromRules,
    getARules,
    hasBuiltinRule,
    makeRule,
    mapABodies,
    mapABodiesArgs,
    mapBodies,
    printRule,
    printRules,
    ruleAllFreeVars,
    ruleFreeVars,
    ruleFreeVars',
    rulesFromARules
    )where

import Data.Monoid
import Data.Typeable
import List
import Monad(liftM)
import Control.Monad.Trans
import Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom(toAtom,fromAtom,Atom)
import Binary
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.Eval
import E.Show
import E.Values
import E.Subst
import E.TypeCheck
import GenUtil
import MapBinaryInstance()
import Name.Name
import Name.Names
import qualified CharIO
import qualified Info.Info as Info
import Info.Types
import Stats
import Support.CanType
import Support.FreeVars
import Util.HasSize




data Rule = Rule {
    ruleHead :: TVr,
    ruleBinds :: [TVr],
    ruleArgs :: [E],
    ruleNArgs :: {-# UNPACK #-} !Int,
    ruleBody :: E,
    ruleUniq :: (Module,Int),
    ruleName :: Atom
    }
 {-! derive: GhcBinary !-}


instance Show Rule where
    showsPrec _ r = showString (fromAtom $ ruleName r)

emptyRule :: Rule
emptyRule = Rule {
    ruleHead = error "ruleHead undefined",
    ruleArgs = [],
    ruleNArgs = 0,
    ruleBinds = [],
    ruleBody = error "ruleBody undefined",
    ruleName = error "ruleName undefined",
    ruleUniq = error "ruleUniq undefined"
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
    freeVars rule = freeVars (ruleBody rule) Set.\\ Set.fromList (ruleBinds rule)

instance FreeVars Rule (Set.Set Id) where
    freeVars rule = freeVars (ruleBody rule) Set.\\ Set.fromList (map tvrIdent $ ruleBinds rule)

instance FreeVars Rule [Id] where
    freeVars rule = Set.toList $ freeVars rule

printRules (Rules rules) = mapM_ printRule (concat $ Map.elems rules)

printRule Rule {ruleName = n, ruleBinds = vs, ruleBody = e2, ruleHead = head, ruleArgs = args } = do
    let e1 = foldl EAp (EVar head) args
    let p v = parens $ pprint v <> text "::" <> pprint (getType v)
    putDocMLn CharIO.putStr $  (tshow n) <+> text "forall" <+> hsep (map p vs) <+> text "."
    let ty = pprint $ getType e1 -- case inferType dataTable [] e1 of
    --    ty2 = pprint $ getType e2
    putDocMLn CharIO.putStr (indent 2 (pprint e1))
    putDocMLn CharIO.putStr $ text " ====>"
    putDocMLn CharIO.putStr (indent 2 (pprint e2))
    putDocMLn CharIO.putStr (indent 2 (text "::" <+> ty))
    --putDocMLn CharIO.putStr (indent 2 (text "::" <+> ty2))

combineRules as bs = map head $ sortGroupUnder ruleUniq (as ++ bs)

instance Monoid Rules where
    mempty = Rules mempty
    mappend (Rules x) (Rules y) = Rules $ Map.unionWith (combineRules) x y


fromRules :: [Rule] -> Rules
fromRules rs = Rules $ Map.map snds $ Map.fromList $ sortGroupUnderF fst [ (tvrIdent $ ruleHead r,r) | r <- rs ]

getARules :: Monad m => Rules -> Id -> m ARules
getARules (Rules mp) tvr = liftM arules (Map.lookup tvr mp)

mapABodies :: Monad m => (E -> m E) -> ARules -> m ARules
mapABodies g (ARules rs) = do
    let f rule = do
            b <- g (ruleBody rule)
            return rule { ruleBody = b }
    rs' <- mapM f rs
    return $ arules $ rs'

mapABodiesArgs :: Monad m => (E -> m E) -> ARules -> m ARules
mapABodiesArgs g (ARules rs) = do
    let f rule = do
            b <- g (ruleBody rule)
            as <- mapM g (ruleArgs rule)
            return rule { ruleArgs = as, ruleBody = b }
    rs' <- mapM f rs
    return $ arules $ rs'

rulesFromARules :: ARules -> [Rule]
rulesFromARules (ARules rs) = rs

-- replace the given arguments with the E values, dropping impossible rules
dropArguments :: [(Int,E)] -> ARules -> ARules
dropArguments os (ARules rs) = arules (catMaybes $  map f rs) where
    f r = do
        let g (i,a) | Just v <- lookup i os = do
                rs <- match (const Nothing) (ruleBinds r) a v
                return (Right rs)
            g (i,a) = return (Left a)
        as' <- mapM g $ zip naturals (ruleArgs r)
        let sb = substLet (concat $ rights as')
            sa = substMap $ Map.fromList [ (tvrIdent t,v) |  Right ds <- as', (t,v) <- ds ]
        return r { ruleArgs = map sa (lefts as'), ruleBody = sb (ruleBody r) }

-- | invarients for ARules
-- sorted by number of arguments rule takes
-- all hidden rule fields filled in

newtype ARules = ARules [Rule]
    deriving(Show,Typeable)

arules xs = ARules (sortUnder ruleNArgs (map f xs)) where
    f rule = rule {
        ruleNArgs = length  (ruleArgs rule),
        ruleBinds = bs,
        ruleBody = g (ruleBody rule),
        ruleArgs = map g (ruleArgs rule)
        } where
        bs = map (setProperty prop_RULEBINDER) (ruleBinds rule)
        g e = substMap (Map.fromList [ (tvrNum t, EVar t) | t <- bs ]) e

instance Monoid ARules where
    mempty = ARules []
    mappend = joinARules


joinARules ar@(ARules a) br@(ARules b)
    | [] <- rs = ARules []
    | all (== r) rs = ARules (sortUnder (\r -> (ruleNArgs r,ruleUniq r)) (snubUnder ruleUniq $ a ++ b))
    | otherwise = error $ "mixing rules!" ++ show (ar,br) where
   rs@(r:_) = map ruleHead a ++ map ruleHead b


applyRules lup (ARules rs) xs = f rs where
    lxs = length xs
    f [] = return Nothing
    f (r:_) | ruleNArgs r > lxs = return Nothing
    f (r:rs) = case sequence (zipWith (match lup (ruleBinds r)) (ruleArgs r) xs) of
        Just ss -> do
            mtick (ruleName r)
            let b = substMap (Map.fromList [ (i,x) | (TVr { tvrIdent = i },x) <- concat ss ]) (ruleBody r)
            return $ Just (b,drop (ruleNArgs r) xs)
        Nothing -> do f rs

preludeError = toId v_error
ruleError = toAtom "Rule.error/EError"

hasBuiltinRule TVr { tvrIdent = n } = n `Set.member` Set.fromList [preludeError]
builtinRule TVr { tvrIdent = n } (ty:s:rs)
    | n == preludeError, Just s' <- toString s  = do
        mtick ruleError
        return $ Just ((EError ("Prelude.error: " ++ s') ty),rs)
builtinRule _ _ = return Nothing


makeRule ::
    String      -- ^ the rule name
    -> (Module,Int)  -- ^ a unique name for this rule
    -> [TVr]    -- ^ the free variables
    -> TVr      -- ^ the head
    -> [E]      -- ^ the args
    -> E        -- ^ the body
    -> Rules
makeRule name uniq fvs head args body = fromRules [rule] where
    rule = emptyRule {
        ruleHead = head,
        ruleBinds = fvs,
        ruleArgs = args,
        ruleNArgs = length args,
        ruleBody = body,
        ruleUniq = uniq,
        ruleName = toAtom $ "Rule.User." ++ name
        }

-- | this determines all free variables of a definition taking rules into account
bindingFreeVars t e = freeVars (tvrType t) `mappend` freeVars e `mappend` freeVars (Info.fetch (tvrInfo t) :: ARules)

