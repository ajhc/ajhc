module E.Rules(
    ARules,
    Rule(Rule,ruleHead,ruleBinds,ruleArgs,ruleBody,ruleUniq,ruleName),
    RuleType(..),
    Rules,
    applyRules,
    arules,
    bindingFreeVars,
    builtinRule,
    dropArguments,
    fromRules,
    getARules,
    mapRules,
    makeRule,
    mapABodiesArgs,
    ruleHeadFreeVars,
    mapBodies,
    printRules,
    ruleAllFreeVars,
    rulesFromARules
    )where

import Data.Monoid
import Data.Typeable
import Data.FunctorM
import List
import Monad(liftM)
import Control.Monad.Trans
import Maybe

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
import Name.Id
import Options
import Util.SetLike as S



data RuleType = RuleSpecialization | RuleUser | RuleCatalyst
    deriving(Eq)
 {-! derive: GhcBinary !-}

-- a rule in its user visible form

data Rule = Rule {
    ruleHead :: TVr,
    ruleBinds :: [TVr],
    ruleArgs :: [E],
    ruleNArgs :: {-# UNPACK #-} !Int,
    ruleBody :: E,
    ruleType :: RuleType,
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
    ruleType = RuleUser,
    ruleBody = error "ruleBody undefined",
    ruleName = error "ruleName undefined",
    ruleUniq = error "ruleUniq undefined"
    }

-- a collection of rules

newtype Rules = Rules (IdMap [Rule])
    deriving(HasSize)


instance Binary Rules where
    put_ h (Rules mp) = putNList h (concat $ melems mp)
    get h = do
        rs <- getNList h
        return $ fromRules rs

mapBodies :: Monad m => (E -> m E) -> Rules -> m Rules
mapBodies g (Rules mp) = do
    let f rule = do
            b <- g (ruleBody rule)
            return rule { ruleBody = b }
    mp' <- fmapM (mapM f) mp
    return $ Rules mp'
    --mp' <- sequence [ do rs' <- mapM f rs; return (k,rs') | (k,rs) <- Map.toAscList mp ]
    --return $ Rules $ Map.fromAscList mp'


ruleAllFreeVars :: Rules -> IdSet
ruleAllFreeVars (Rules r) = freeVars (melems r)


ruleHeadFreeVars :: Rules -> IdSet
ruleHeadFreeVars (Rules rs) = unions $ map f (concat $ melems rs) where
    f r = (S.insert (tvrIdent $ ruleHead r) $ freeVars (ruleArgs r)) S.\\ fromList (map tvrIdent $ ruleBinds r)


instance FreeVars ARules IdSet where
    freeVars a = aruleFreeVars a

instance FreeVars Rule IdSet where
    freeVars rule = freeVars (ruleBody rule) S.\\ fromList (map tvrIdent $ ruleBinds rule)
instance FreeVars Rule (IdMap TVr) where
    freeVars rule = freeVars (ruleBody rule) S.\\ fromList [ (tvrIdent t,t) | t <- ruleBinds rule]

instance FreeVars Rule [Id] where
    freeVars rule = idSetToList $ freeVars rule

{-# NOINLINE printRules #-}
printRules ty (Rules rules) = mapM_ (\r -> printRule r >> putChar '\n') [ r | r <- concat $ melems rules, ruleType r == ty ]

putDocMLn' :: Monad m => (String -> m ()) -> Doc -> m ()
putDocMLn' putStr d = displayM putStr (renderPretty 0.80 (optColumns options) d) >> putStr "\n"

printRule Rule {ruleName = n, ruleBinds = vs, ruleBody = e2, ruleHead = head, ruleArgs = args } = do
    let e1 = foldl EAp (EVar head) args
    let p v = parens $ pprint v <> text "::" <> pprint (getType v)
    putDocMLn' CharIO.putStr $  (tshow n) <+> text "forall" <+> hsep (map p vs) <+> text "."
    let ty = pprint $ getType e1 -- case inferType dataTable [] e1 of
    --    ty2 = pprint $ getType e2
    putDocMLn' CharIO.putStr (indent 2 (pprint e1) <+> text "::" <+> ty )
    putDocMLn' CharIO.putStr $ text " ==>" <+> pprint e2
    --putDocMLn CharIO.putStr (indent 2 (pprint e2))
    --putDocMLn CharIO.putStr (indent 2 (text "::" <+> ty2))

combineRules as bs = map head $ sortGroupUnder ruleUniq (as ++ bs)

instance Monoid Rules where
    mempty = Rules mempty
    mappend (Rules x) (Rules y) = Rules $ munionWith (combineRules) x y


fromRules :: [Rule] -> Rules
fromRules rs = Rules $ fmap snds $ fromList $ sortGroupUnderF fst [ (tvrIdent $ ruleHead r,r) | r <- rs ]

getARules :: Monad m => Rules -> Id -> m ARules
getARules (Rules mp) tvr = liftM arules (mlookup tvr mp)

mapABodies :: Monad m => (E -> m E) -> ARules -> m ARules
mapABodies g ARules { aruleRules = rs } = do
    let f rule = do
            b <- g (ruleBody rule)
            return rule { ruleBody = b }
    rs' <- mapM f rs
    return $ arules $ rs'

mapABodiesArgs :: Monad m => (E -> m E) -> ARules -> m ARules
mapABodiesArgs g ARules { aruleRules = rs } = do
    let f rule = do
            b <- g (ruleBody rule)
            as <- mapM g (ruleArgs rule)
            return rule { ruleArgs = as, ruleBody = b }
    rs' <- mapM f rs
    return $ arules $ rs'

rulesFromARules :: ARules -> [Rule]
rulesFromARules = aruleRules

mapRules :: Monad m => (Rule -> m Rule) -> ARules -> m ARules
mapRules f ARules { aruleRules = rules } = do
    rs <- mapM f rules
    return $ arules rs

-- replace the given arguments with the E values, dropping impossible rules
dropArguments :: [(Int,E)] -> ARules -> ARules
dropArguments os ARules { aruleRules = rs } = arules (catMaybes $  map f rs) where
    f r = do
        let g (i,a) | Just v <- lookup i os = do
                rs <- match (const Nothing) (ruleBinds r) a v
                return (Right rs)
            g (i,a) = return (Left a)
        as' <- mapM g $ zip naturals (ruleArgs r)
        let sb = substLet (concat $ rights as')
            sa = substMap $ fromList [ (tvrIdent t,v) |  Right ds <- as', (t,v) <- ds ]
        return r { ruleArgs = map sa (lefts as'), ruleBody = sb (ruleBody r) }

-- | ARules contains a set of rules for a single id, optimized for fast application
--
-- invarients for ARules
-- sorted by number of arguments rule takes
-- all hidden rule fields filled in
-- free variables are up to date



data ARules = ARules {
    aruleFreeVars :: IdSet,
    aruleRules :: [Rule]
    }
    deriving(Typeable)

instance Show ARules where
    showsPrec n a = showsPrec n (aruleRules a)

arules xs = ARules { aruleFreeVars = freeVars rs, aruleRules = rs } where
    rs = sortUnder ruleNArgs (map f xs)
    f rule = rule {
        ruleNArgs = length  (ruleArgs rule),
        ruleBinds = bs,
        ruleBody = g (ruleBody rule),
        ruleArgs = map g (ruleArgs rule)
        } where
        bs = map (setProperty prop_RULEBINDER) (ruleBinds rule)
        g e = substMap (fromList [ (tvrIdent t, EVar t) | t <- bs ]) e

instance Monoid ARules where
    mempty = ARules { aruleFreeVars = mempty, aruleRules = [] }
    mappend = joinARules


joinARules ar@(ARules fvsa a) br@(ARules fvsb b)
    | [] <- rs = ARules mempty []
    | all (== r) rs = ARules (fvsa `mappend` fvsb) (sortUnder (\r -> (ruleNArgs r,ruleUniq r)) (snubUnder ruleUniq $ a ++ b))
    | otherwise = error $ "mixing rules!" ++ show (ar,br) where
   rs@(r:_) = map ruleHead a ++ map ruleHead b

rsubstMap :: IdMap E -> E -> E
rsubstMap im e = doSubst False True (fmap ( (`mlookup` im) . tvrIdent) (unions $ (freeVars e :: IdMap TVr):map freeVars (melems im))) e

applyRules lup (ARules _ rs) xs = f rs where
    lxs = length xs
    f [] = return Nothing
    f (r:_) | ruleNArgs r > lxs = return Nothing
    f (r:rs) = case sequence (zipWith (match lup (ruleBinds r)) (ruleArgs r) xs) of
        Just ss -> do
            mtick (ruleName r)
            let b = rsubstMap (fromList [ (i,x) | (TVr { tvrIdent = i },x) <- concat ss ]) (ruleBody r)
            return $ Just (b,drop (ruleNArgs r) xs)
        Nothing -> do f rs

preludeError = toId v_error
ruleError = toAtom "Rule.error/EError"

builtinRule TVr { tvrIdent = n } (ty:s:rs)
    | n == preludeError, Just s' <- toString s  = do
        mtick ruleError
        return $ Just ((EError ("Prelude.error: " ++ s') ty),rs)
builtinRule _ _ = return Nothing


makeRule ::
    String      -- ^ the rule name
    -> (Module,Int)  -- ^ a unique name for this rule
    -> RuleType -- ^ type of rule
    -> [TVr]    -- ^ the free variables
    -> TVr      -- ^ the head
    -> [E]      -- ^ the args
    -> E        -- ^ the body
    -> Rule
makeRule name uniq ruleType fvs head args body = rule where
    rule = emptyRule {
        ruleHead = head,
        ruleBinds = fvs,
        ruleArgs = args,
        ruleType = ruleType,
        ruleNArgs = length args,
        ruleBody = body,
        ruleUniq = uniq,
        ruleName = toAtom $ "Rule.User." ++ name
        }

-- | this determines all free variables of a definition taking rules into account
bindingFreeVars :: TVr -> E -> IdSet
bindingFreeVars t e = freeVars (tvrType t) `mappend` freeVars e `mappend` freeVars (Info.fetch (tvrInfo t) :: ARules)

