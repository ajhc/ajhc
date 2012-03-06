module E.Rules(
    ARules,
    Rule(Rule,ruleHead,ruleBinds,ruleArgs,ruleBody,ruleUniq,ruleName),
    RuleType(..),
    Rules(..),
    applyRules,
    arules,
    builtinRule,
    dropArguments,
    fromRules,
    ruleUpdate,
    mapRBodyArgs,
    makeRule,
    mapBodies,
    printRules,
    rulesFromARules
    )where

import Control.Monad.Writer(WriterT(..),execWriterT,liftM,tell)
import Data.Maybe
import Data.Monoid(Monoid(..))
import qualified Data.Traversable as T

import Data.Binary
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.Binary()
import E.E
import E.Show()
import E.Subst
import E.Values
import GenUtil
import Info.Types
import Name.Id
import Name.Name
import Name.Names
import Options
import Stats
import StringTable.Atom(toAtom)
import Support.CanType
import Support.FreeVars
import Support.MapBinaryInstance()
import Util.HasSize
import Util.SetLike as S
import qualified Util.Seq as Seq

instance Show Rule where
    showsPrec _ r = shows $ ruleName r

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
    deriving(HasSize,IsEmpty)

instance Eq Rule where
    r1 == r2 = ruleUniq r1 == ruleUniq r2

instance Binary Rules where
    put (Rules mp) = put (concat $ values mp)
    get = do
        rs <- get
        return $ fromRules rs

mapBodies :: Monad m => (E -> m E) -> Rules -> m Rules
mapBodies g (Rules mp) = do
    let f rule = do
            b <- g (ruleBody rule)
            return rule { ruleBody = b }
    mp' <- T.mapM (mapM f) mp
    return $ Rules mp'
    --mp' <- sequence [ do rs' <- mapM f rs; return (k,rs') | (k,rs) <- Map.toAscList mp ]
    --return $ Rules $ Map.fromAscList mp'

instance FreeVars Rule [Id] where
    freeVars rule = idSetToList $ freeVars rule

{-# NOINLINE printRules #-}
printRules ty (Rules rules) = mapM_ (\r -> printRule r >> putChar '\n') [ r | r <- concat $ values rules, ruleType r == ty ]

putDocMLn' :: Monad m => (String -> m ()) -> Doc -> m ()
putDocMLn' putStr d = displayM putStr (renderPretty 0.80 (optColumns options) d) >> putStr "\n"

printRule Rule {ruleName = n, ruleBinds = vs, ruleBody = e2, ruleHead = head, ruleArgs = args } = do
    let e1 = foldl EAp (EVar head) args
    let p v = parens $ pprint v <> text "::" <> pprint (getType v)
    putDocMLn' putStr $  (tshow n) <+> text "forall" <+> hsep (map p vs) <+> text "."
    let ty = pprint $ getType e1 -- case inferType dataTable [] e1 of
    --    ty2 = pprint $ getType e2
    putDocMLn' putStr (indent 2 (pprint e1) <+> text "::" <+> ty )
    putDocMLn' putStr $ text " ==>" <+> pprint e2
    --putDocMLn CharIO.putStr (indent 2 (pprint e2))
    --putDocMLn CharIO.putStr (indent 2 (text "::" <+> ty2))

combineRules as bs = map head $ sortGroupUnder ruleUniq (as ++ bs)

instance Monoid Rules where
    mempty = Rules mempty
    mappend (Rules x) (Rules y) = Rules $ unionWith combineRules x y

fromRules :: [Rule] -> Rules
fromRules rs = Rules $ fmap snds $ fromList $ sortGroupUnderF fst [ (tvrIdent $ ruleHead r,ruleUpdate r) | r <- rs ]

mapRBodyArgs :: Monad m => (E -> m E) -> Rule -> m Rule
mapRBodyArgs g r = do
    let f rule = do
            b <- g (ruleBody rule)
            as <- mapM g (ruleArgs rule)
            return rule { ruleArgs = as, ruleBody = b }
    f r

rulesFromARules :: ARules -> [Rule]
rulesFromARules = aruleRules

-- replace the given arguments with the E values, dropping impossible rules
dropArguments :: [(Int,E)] -> [Rule] -> [Rule]
dropArguments os  rs  = catMaybes $  map f rs where
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

ruleUpdate rule = rule {
        ruleNArgs = length  (ruleArgs rule),
        ruleBinds = bs,
        ruleBody = g (ruleBody rule),
        ruleArgs = map g (ruleArgs rule)
    } where
        bs = map (setProperty prop_RULEBINDER) (ruleBinds rule)
        g e = substMap (fromList [ (tvrIdent t, EVar t) | t <- bs ]) e

joinARules ar@(ARules fvsa a) br@(ARules fvsb b)
    | [] <- rs = ARules mempty []
    | all (== r) rs = ARules (fvsa `mappend` fvsb) (sortUnder (\r -> (ruleNArgs r,ruleUniq r)) (snubUnder ruleUniq $ a ++ b))
    | otherwise = error $ "mixing rules!" ++ show (ar,br) where
   rs@(r:_) = map ruleHead a ++ map ruleHead b

rsubstMap :: IdMap E -> E -> E
rsubstMap im e = doSubst False True (fmap ( (`mlookup` im) . tvrIdent) (unions $ (freeVars e :: IdMap TVr):map freeVars (values im))) e

applyRules :: MonadStats m => (Id -> Maybe E) -> ARules -> [E] -> m (Maybe (E,[E]))
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

-- | find substitution that will transform the left term into the right one,
-- only substituting for the vars in the list

match :: Monad m =>
    (Id -> Maybe E)      -- ^ function to look up values in the environment
    -> [TVr]              -- ^ vars which may be substituted
    -> E                  -- ^ pattern to match
    -> E                  -- ^ input expression
    -> m [(TVr,E)]
match lup vs = \e1 e2 -> liftM Seq.toList $ execWriterT (un e1 e2 () etherealIds) where
    bvs :: IdSet
    bvs = fromList (map tvrIdent vs)

    un _ _ _ c | c `seq` False = undefined
    un (EAp a b) (EAp a' b') mm c = do
        un a a' mm c
        un b b' mm c
    un (ELam va ea) (ELam vb eb) mm c = lam va ea vb eb mm c
    un (EPi va ea) (EPi vb eb) mm c = lam va ea vb eb mm c
    un (EPrim s xs t) (EPrim s' ys t') mm c | length xs == length ys = do
        sequence_ [ un x y mm c | x <- xs | y <- ys]
        un t t' mm c
    un (ESort x) (ESort y) mm c | x == y = return ()
    un (ELit (LitInt x t1))  (ELit (LitInt y t2)) mm c | x == y = un t1 t2 mm c
    un (ELit LitCons { litName = n, litArgs = xs, litType = t })  (ELit LitCons { litName = n', litArgs = ys, litType =  t'}) mm c | n == n' && length xs == length ys = do
        sequence_ [ un x y mm c | x <- xs | y <- ys]
        un t t' mm c

    un (EVar TVr { tvrIdent = i, tvrType =  t}) (EVar TVr {tvrIdent = j, tvrType =  u}) mm c | i == j = un t u mm c
    un (EVar TVr { tvrIdent = i, tvrType =  t}) (EVar TVr {tvrIdent = j, tvrType =  u}) mm c | isEtherealId i || isEtherealId j  = fail "Expressions don't match"
    un (EVar tvr@TVr { tvrIdent = i, tvrType = t}) b mm c
        | i `member` bvs = tell (Seq.single (tvr,b))
        | otherwise = fail $ "Expressions do not unify: " ++ show tvr ++ show b
    un a (EVar tvr) mm c | Just b <- lup (tvrIdent tvr), not $ isEVar b = un a b mm c

    un a b _ _ = fail $ "Expressions do not unify: " ++ show a ++ show b
    lam va ea vb eb mm ~(c:cs) = do
        un (tvrType va) (tvrType vb) mm (c:cs)
        un (subst va (EVar va { tvrIdent = c }) ea) (subst vb (EVar vb { tvrIdent = c }) eb) mm cs
