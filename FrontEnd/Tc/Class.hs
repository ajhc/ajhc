module FrontEnd.Tc.Class(
    Pred,
    ClassHierarchy(),
    splitPreds,
    generalize,
    splitReduce,
    topDefaults,
    freeMetaVarsPreds,
    simplify,
    assertEntailment,
    assertEquivalant,
    Preds
    )where

import Control.Monad.Trans
import Data.Monoid
import List
import Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Representation(Class)

import Name.Names
import Options
import Doc.DocLike
import Support.CanType
import qualified FlagOpts as FO
import qualified FlagDump as FD
import FrontEnd.Class
import FrontEnd.Tc.Type
import FrontEnd.Tc.Monad
import Doc.PPrint
import Warning



generalize :: [Pred] -> Rho -> Tc Sigma
generalize ps r = do
    ch <- getClassHierarchy
    r <- flattenType r
    fmvenv <- freeMetaVarsEnv
    let mvs =  nub [ v  | v <- freeMetaVars r, not $ v `Set.member` fmvenv ]
    --(nps,rp) <- splitPreds ch (Set.toList fmvenv) ps
    (mvs',nps,rp) <- splitReduce (Set.toList fmvenv) mvs (simplify ch ps)
    addPreds nps
    quantify mvs' rp r

freeMetaVarsPreds :: Preds -> [MetaVar]
freeMetaVarsPreds ps = concatMap freeMetaVarsPred ps

freeMetaVarsPred :: Pred -> [MetaVar]
freeMetaVarsPred (IsIn _ t) = freeMetaVars t
freeMetaVarsPred (IsEq t1 t2) = freeMetaVars t1 ++ freeMetaVars t2

-- | split predicates into ones that only mention metavars in the list vs other ones
splitPreds :: Monad m => ClassHierarchy -> [MetaVar] -> Preds -> m (Preds, Preds)
splitPreds h fs ps  = do
    ps' <- toHnfs h ps
    return $ partition (all (`elem` fs) . freeMetaVarsPred) $ simplify h  $ ps'

toHnfs      :: Monad m => ClassHierarchy -> [Pred] -> m [Pred]
toHnfs h ps =  mapM (toHnf h) ps >>= return . concat

toHnf :: Monad m => ClassHierarchy -> Pred -> m [Pred]
toHnf h p
    | inHnf p = return [p]
    | otherwise =  case reducePred h p of
         Nothing -> fail $ "context reduction, no instance for: "  ++ (pprint  p)
         Just ps -> toHnfs h ps

inHnf       :: Pred -> Bool
inHnf (IsIn c t) = hnf t
 where hnf (TVar v)  = True
       hnf TMetaVar {} = True
       hnf (TCon tc) = False
       hnf (TAp t _) = hnf t
       hnf (TArrow _t1 _t2) = False
       hnf TForAll {} = False
       hnf TExists {} = False

reducePred :: Monad m => ClassHierarchy -> Pred -> m [Pred]
reducePred h p@(IsIn c t)
    | Just x <- foldr mplus Nothing poss = return x
    | otherwise = fail "reducePred"
 where poss = map (byInst p) (instsOf h c)


simplify :: ClassHierarchy -> [Pred] -> [Pred]
simplify h ps = loop [] ps where
    loop rs []     = rs
    loop rs (p:ps)
        | entails h (rs ++ ps) p = loop rs ps
        | otherwise = loop (p:rs) ps


-- | returns true when set of predicates implies some other predicate is satisfied.
entails :: ClassHierarchy -> [Pred] -> Pred -> Bool
entails h ps p = (p `elem` concatMap (bySuper h) ps) ||
           case reducePred h p of
             Nothing -> False
             Just qs -> all (entails h ps) qs

bySuper :: ClassHierarchy -> Pred -> [Pred]
bySuper h p@(IsIn c t)
 = p : concatMap (bySuper h) supers
   where supers = [ IsIn c' t | c' <- supersOf h c ]

byInst             :: Monad m => Pred -> Inst -> m [Pred]
byInst p Inst { instHead = ps :=> h } = do
    u <- matchPred h p
    return (map (inst mempty (Map.fromList [ (tyvarAtom mv,t) | (mv,t) <- u ])) ps)

matchPred :: Monad m => Pred -> Pred -> m [(Tyvar,Type)]
matchPred x@(IsIn c t) y@(IsIn c' t')
      | c == c'   = match t t'
      | otherwise = fail $ "Classes do not match: " ++ show (x,y)

supersOf :: ClassHierarchy -> Class -> [Class]
supersOf ch c = asksClassRecord ch c classSupers
instsOf :: ClassHierarchy -> Class -> [Inst]
instsOf ch c = asksClassRecord ch c classInsts


match :: Monad m => Type -> Type -> m [(Tyvar,Type)]
match x y = do match' x y
match' (TAp l r) (TAp l' r') = do
    sl <- match l l'
    sr <- match r r'
    return $ mappend sl sr
match' (TArrow l r) (TArrow l' r') = do
    sl <- match l l'
    sr <- match r r'
    return $ mappend sl sr
match' (TVar u) (TVar t) | u == t = return mempty
match' (TVar mv) t | getType mv == getType t = return [(mv,t)]
--match' (TMetaVar mv) t | kind mv == kind t = return [(mv,t)]
match' (TCon tc1) (TCon tc2) | tc1==tc2 = return mempty
match' t1 t2  = fail $ "match: " ++ show (t1,t2)


splitReduce :: [MetaVar] -> [MetaVar] -> [Pred] -> Tc ([MetaVar],[Pred], [Pred])
splitReduce fs gs ps = do
    h <- getClassHierarchy
    --liftIO $ putStrLn $ pprint (fs,gs,ps)
    (ds, rs) <- splitPreds h fs ps
    --liftIO $ putStrLn $ pprint (ds,rs)
    (rs',sub) <- genDefaults h (fs++gs) rs
    --liftIO $ putStrLn $ pprint (rs')
    flip mapM_ sub $ \ (x,y) ->  do
        wdump FD.BoxySteps $ liftIO $ putStrLn $ "defaulting: " <+> pprint x <+> "=>" <+> prettyPrintType y
        addWarn "type-defaults" ("defaulting: " <+> pprint x <+> "=>" <+> prettyPrintType y)
    sequence_ [ varBind x y | (x,y) <- nub sub]
    return (nub gs List.\\ map fst sub, ds,rs')

withDefaults     :: Monad m => ClassHierarchy ->  [MetaVar] -> [Pred] -> m [(MetaVar, [Pred], Type)]
withDefaults h vs ps
  | any null tss = fail $ "withDefaults.ambiguity: " ++ (pprint ps)  ++ pprint vs -- ++ show ps
--  | otherwise = fail $ "Zambiguity: " ++ (render $ pprint ps) ++  show (ps,ps',ams)
  | otherwise    = return $ [ (v,qs,head ts) | (v,qs,ts) <- ams ]
    where ams = ambig h vs ps
          tss = [ ts | (v,qs,ts) <- ams ]

-- Return retained predicates and a defaulting substitution
genDefaults :: Monad m => ClassHierarchy ->  [MetaVar] -> [Pred] -> m ([Pred],[(MetaVar,Type)])
genDefaults h vs ps = do
    ams <- withDefaults h vs ps
    let ps' = [ p | (v,qs,ts) <- ams, p<-qs ]
        vs  = [ (v,t)  | (v,qs,t) <- ams ]
    return (ps \\ ps',  vs)

-- ambiguities from THIH + call to candidates
ambig :: ClassHierarchy -> [MetaVar] -> [Pred] -> [(MetaVar,[Pred],[Type])]

ambig h vs ps
  = [ (v, qs, defs h v qs) |
         v <- nub (freeMetaVarsPreds ps) \\ vs,
         let qs = [ p | p<-ps, v `elem` freeMetaVarsPred p ] ]


assertEntailment :: Preds -> Preds -> Tc ()
assertEntailment qs ps = do
    ch <- getClassHierarchy
    let ns = [ p  | p <- ps, not $ entails ch qs p ]
    if null ns then return () else
        fail $ "Signature too Weak: " ++ pprint qs ++ " does not imply " ++ pprint ns

assertEquivalant :: Preds -> Preds -> Tc ()
assertEquivalant qs ps = do
    assertEntailment qs ps
    assertEntailment ps qs
{-

reduce :: OptionMonad m => ClassHierarchy -> [Tyvar] -> [Tyvar] -> [Pred] -> m ([Pred], [Pred])

reduce h fs gs ps = do
    (ds, rs) <- split h fs ps
    rs' <-   useDefaults h (fs++gs) rs
    return (ds,rs')
-}

-- 'candidates' from THIH
defs     :: ClassHierarchy -> MetaVar -> [Pred] -> [Type]
defs h v qs = [ t | all ((TMetaVar v)==) ts,
                  all (`elem` stdClasses) cs, -- XXX needs fixing
                  any (`elem` numClasses) cs, -- XXX needs fixing
                  t <- defaults, -- XXX needs fixing
                  and [ entails h [] (IsIn c t) | c <- cs ]]
 where cs = [ c | (IsIn c t) <- qs ]
       ts = [ t | (IsIn c t) <- qs ]


defaults    :: [Type]
defaults
    | not $ fopts FO.Defaulting = []
    | otherwise = map (\name -> TCon (Tycon name Star)) [tc_Integer, tc_Double]

topDefaults     :: [Pred] -> Tc ()
topDefaults ps  = do
    h <- getClassHierarchy
    let ams = ambig h [] ps
        tss = [ ts | (v,qs,ts) <- ams ]
        vs  = [ v  | (v,qs,ts) <- ams ]
    when (any null tss) $ fail $ "Top Level ambiguity " ++ (pprint ps)
    return ()
--      | otherwise    -> return $ Map.fromList (zip vs (map head tss))
--        where ams = ambig h [] ps
--              tss = [ ts | (v,qs,ts) <- ams ]
--              vs  = [ v  | (v,qs,ts) <- ams ]
