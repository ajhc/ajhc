module E.Simplify(simplify,SimpOpts(..)) where


import Atom
import Control.Monad.Trans
import DataConstructors
import Data.Monoid
import E.E
import E.LetFloat
import E.PrimOpt
import E.Rules
import E.Subst
import E.Traverse
import E.TypeCheck
import E.Values
import FreeVars
import GenUtil
import HasSize
import List
import Monad
import Name
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Stats
import E.Strictness as Strict
import CanType

inlineThreshold = 7

shouldInline ELam {} [] = False  -- Lambda lifting will undo anyway
shouldInline exp xs = safeToDup exp && size e + length as - length xs - sum (map (fromEnum . isFullyConst) xs) - 4 * fromEnum (whnfOrBot e) - 5 * isCombinator exp  < inlineThreshold   where
    (e,as) = fromLam exp
    isCombinator e = fromEnum $ IntMap.size (freeVs e) == 0

--patBinds  PatWildCard  = 1
--patBinds (PatLit (LitCons _ xs _)) = length xs
--patBinds _ = 0

--armMap :: (E -> E) -> Alt E -> Alt E
--armMap f (p,e) | nb <= length ts = (p,foldr ELam (f e'') tsa) where 
--    (e',ts) = fromLam e
--    (tsa,tsb) = splitAt nb ts
--    e'' = foldr ELam e' tsb
--    nb = patBinds p 

doSimplify sopts  stats (x,xs) = f (x,xs) where  
    funcProps = so_properties sopts
    dataTable = so_dataTable sopts
--    f (ELam (TVr Nothing _) e,a:as) = do
--        tick stats (toAtom "E.Simplify.beta-reduce") 
--        f (e,as)
--    f (EPi (TVr Nothing _) e,a:as) = do
--        tick stats (toAtom "E.Simplify.pi-reduce") 
--        f (e,as)
    f (p@EPrim {},xs) = do
        p' <- primOpt dataTable stats p 
        cont (p',xs)
    f (ELam tvr e,(a:as)) = do
        tick stats (toAtom "E.Simplify.beta-reduce") 
        f (subst tvr a e,as)   
    f (EPi tvr e,(a:as)) = do
        tick stats (toAtom "E.Simplify.pi-reduce") 
        f (subst tvr a e,as)   
    f (ECase e b as d,xs@(_:_)) = do
        tick stats (toAtom "E.Simplify.case-application") 
        f (ECase e b (map (\(Alt p e) -> Alt p $ foldl EAp e xs) as) (fmap (\e -> foldl EAp e xs) d) ,[])
    f (ec@(ECase e _ _ _),[]) | isBottom e = do
        tick stats (toAtom "E.Simplify.case-of-bottom") 
        f (prim_unsafeCoerce e (typ ec),[])
    f (ECase e b as (Just (ECase e' b' as' d')),[]) | e == e' = do 
        tick stats (toAtom "E.Simplify.case-merging") 
        let (nb,mdc)   
                | tvrNum b == 0 = (b',id)
                | tvrNum b' == 0 = (b,id)
                | otherwise = (b,ELetRec [(b',EVar b)]) -- error "case-default-case: double bind"
            nas' = filter ( (`notElem` map altHead as) . altHead) as' 
        f (ECase e nb (as ++ nas') (fmap mdc d'),[])
    f (oc@(ECase ic@(ECase e b as d) b' as' d'),[]) | length (filter (not . isBottom) (caseBodies ic)) <= 1 || all whnfOrBot (caseBodies ic) || all whnfOrBot (caseBodies oc) = do
        tick stats (toAtom "E.Simplify.case-of-case") 
        let f (Alt l e) = Alt l (g e)
            g x = ECase x b' as' d'
        cont (ECase e b (map f as) (fmap g d),[])      -- we duplicate code so continue for next renaming pass before going further.
        
    f ec@(ECase e b as@(Alt (LitCons n _ _) _:_) (Just d),[]) | Just ss <- getSiblings dataTable n, length ss <= length as = do
        when (length ss < length as) $ fail ("Bad case: " ++ show ec)
        tick stats (toAtom "E.Simplify.case-no-default") 
        f (ECase e b as Nothing,[])
    f (ECase e b [] (Just d),[]) | not (isLifted e) = do
        tick stats (toAtom "E.Simplify.case-unlifted")
        f (eLet b e d,[]) 
        
    --f (ECase e (TVr 0 _) as (Just (ELetRec ds (ECase e' b' as' d'))),[]) | e == e' = do 
    --    tick stats (toAtom "E.Simplify.case-merging") 
    --    let nas' = filter ( (`notElem` map altHead as) . altHead) as' 
    --    f (ELetRec ds  $ ECase e b' (as ++ nas') d',[])
        
    f (ec@ECase { eCaseScrutinee = el@(ELit l), eCaseAlts = [], eCaseDefault = Just e },[]) | isFullyConst el = do
        tick stats (toAtom "E.Simplify.case-fully-const") 
        cont (subst (eCaseBind ec) el e,[])
    f (ec@ECase { eCaseScrutinee = el@(ELit l) },[]) = do
        (x,as) <- match l (eCaseAlts ec) (eCaseDefault ec)
        cont (eLet (eCaseBind ec) el (foldl eAp x as),[])
        --liftM (mapFst $ eLet (eCaseBind ec) el) $ 
    f (EError s t,xs@(_:_)) = do
        ticks stats (length xs) (toAtom "E.Simplify.error-application") 
        f (EError s (foldl eAp t xs),[])
--    f (ec@ECase { eCaseScrutinee = (EVar tvr)} ,[]) = do
--        e <- lookupBinding tvr
--        case e of 
--            IsBoundTo el@(ELit l) -> liftM (mapFst $ eLet (eCaseBind ec) el) $ match l (eCaseAlts ec) (eCaseDefault ec)
--            NotAmong na | ECase e b [] (Just d) <- ec { eCaseAlts =  filtAlts na $ eCaseAlts ec } ->  do 
--                tick stats (toAtom "E.Simplify.seq-evaled") 
--                f (eLet b e d,[]) 
--    f ec@(ECase e b as@(Alt (LitCons n _ _) _:_) (Just d),[]) | Just ss <- getSiblings dataTable n, length ss <= length as = do
 --       when (length ss < length as) $ fail ("Bad case: " ++ show ec)
 --       tick stats (toAtom "E.Simplify.case-no-default") 
--        f (ECase e b as Nothing,[])
--            _ -> cont (ec,[])
    f (x@(EVar v),xs) = do
        z <- applyRule' stats (so_rules sopts) v xs 
        case z of
            Just (x,xs) -> f (x,xs)
            Nothing -> do
                e <- lookupBinding v
                case e of
                    IsBoundTo exp | forceInline v -> do
                        tick stats (toAtom $ "E.Simplify.inline.forced.{" ++ tvrShowName v  ++ "}")
                        cont (exp,xs)
                    IsBoundTo (EVar v') -> do
                        tick stats (toAtom $ "E.Simplify.inline.copy-propagate.head.{" ++ tvrShowName v' ++ "}") 
                        f (EVar v',xs)
                    IsBoundTo (ELit l) -> do
                        tick stats (toAtom "E.Simplify.inline.constant-folding") 
                        cont (ELit l,xs)
                    IsBoundTo x@(EError s t) -> do
                        tick stats (toAtom "E.Simplify.inline.error-folding") 
                        ticks stats (length xs) (toAtom "E.Simplify.error-application") 
                        f (EError s (foldl eAp t xs),[])
                    IsBoundTo exp 
                        | shouldInline exp xs -> do 
                            let name = tvrShowName v
                                name' = if  ("Instance@." `isPrefixOf` name) then "Instance@" else name 
                            tick stats (toAtom $ "E.Simplify.inline.value.{" ++ name'  ++ "}")
                            cont (exp,xs)
                        | otherwise -> cont (x,xs)
                    _ -> cont (x,xs)
    f (x,xs) = cont (x,xs)
    cont (x,xs) = do
        x <- g' x
        xs <- mapM g' xs
        liftIO $ doCoalesce stats (x,xs)
        --return (x,xs)
    isGood (LitCons _ (_:_) _) = False 
    isGood _ = True
    --match :: Lit E -> [(Pat E,E)] -> IO (E,[E])
    match m@(LitCons c xs _) ((Alt (LitCons c' bs _) e):rs) d | c == c' = do
        tick stats (toAtom $ "E.Simplify.known-case." ++ show c ) 
        cont (ELetRec (zip bs xs) e,[])
            | otherwise = match m rs d
    match m@(LitInt la _) ((Alt (LitInt lb _) e):rs) d | la == lb  = do
        tick stats (toAtom $ "E.Simplify.known-case." ++ show la) 
        f (e,[])
            | otherwise = match m rs d
    match l [] (Just e) = do 
        tick stats (toAtom "E.Simplify.known-case._") 
        f (e,[])
    match m [] Nothing = error $ "End of match: " ++ show m 
    match m as d = error $ "Odd Match: " ++ show ((m,getType m),as,d)
        
        
    g' (EPrim p xs t) = do
        xs' <- mapM g' xs
        return $ EPrim p xs' t
    g' (ELit (LitCons p xs t)) = do
        xs' <- mapM g' xs
        return $ ELit (LitCons p xs' t)
    g' x = do
        (x',[]) <- g (x,[])
        return x'
    g (ELam tvr@(TVr { tvrIdent = n}) e,[]) | n /= 0,  n `notElem` freeVars e = do
        tick stats (toAtom "E.Simplify.blank-lam") 
        return (ELam (tvr {tvrIdent = 0 }) e,[]) 
    g (EPi tvr@(TVr { tvrIdent = n }) e,[]) | n /= 0,  n `notElem` freeVars e = do
        tick stats (toAtom "E.Simplify.blank-pi") 
        return (EPi (tvr { tvrIdent =  0 }) e,[]) 
--    g (EPi (TVr (Just i) _) (EAp a (EVar (TVr (Just i') _))),[]) | i == i' && not (i `elem` freeVars a) = do
--        tick stats (toAtom "E.Simplify.eta-reduce-pi") 
--        g (a,[]) 
--    g (ELam (TVr (Just i) _) (EAp a (EVar (TVr (Just i') _))),[]) | i == i' && not (i `elem` freeVars a) = do
--        tick stats (toAtom "E.Simplify.eta-reduce-lam") 
--        g (a,[]) 
        
    g (x@(EVar v),xs@[]) = do
        e <- lookupBinding v
        case e of
            IsBoundTo (EVar v') -> do
                tick stats (toAtom $ "E.Simplify.inline.copy-propagate.arg.{" ++ tvrShowName v' ++ "}") 
                g (EVar v',xs)
            IsBoundTo e | Just _ <- fullyConst e -> do
                tick stats (toAtom $ "E.Simplify.inline.constant-folding") 
                return (e,xs)
            IsBoundTo e | Just (EVar _,_) <- from_unsafeCoerce e -> do
                tick stats (toAtom "E.Simplify.inline.arg-unsafeCoerce") 
                return (e,xs)
            IsBoundTo (ELit l) | isGood l -> do
                tick stats (toAtom "E.Simplify.inline.constant-folding2") 
                return (ELit l,xs)
            --IsBoundTo x@(EError {}) -> do
            --    tick stats (toAtom "E.Simplify.error-folding") 
            --    return (x,xs)
            --Just z | sortTypeLike z -> do
            --    tick stats (toAtom "E.Simplify.constant-folding") 
            --    f (z,xs)
            _ -> return (x,xs)
    g (x,[]) = return (x,[])
    forceInline x | Just n <- tvrName x, Just xs <- Map.lookup n funcProps  = toAtom "INLINE" `elem` xs
    forceInline _ = False

filtAlts ns (Alt (LitCons n _ _) _:as) | n `elem` ns  = filtAlts ns as
filtAlts ns (a:as) = a:filtAlts ns as
filtAlts ns [] = []

litMatch (LitInt a _) (LitInt b _) = a == b
--litMatch (LitFrac a _) (LitFrac b _) = a == b
litMatch LitCons {} LitCons {} = False -- taken care of above
litMatch x y = error $ "litMatch: " ++ show (x,y)

--propRec stats =  itick stats (toAtom "E.Simplify.inline.copy-propagate") 
ltcRec stats i =  ticks stats i (toAtom "E.Simplify.let-to-case") 


data SimpOpts = SimpOpts {
    so_properties :: Map.Map Name [Atom],
    so_rules :: Rules,
    so_boundVars :: Map.Map Int E, 
    so_dataTable :: DataTable,
    so_strictness :: Map.Map Int Strict.SA
    }
    {-! derive: Monoid !-}

simplify :: SimpOpts -> Stats -> E -> IO E
simplify sopts stats e = traverse travOptions { 
    pruneRecord = varElim stats, 
    trav_rules = so_rules sopts,
    trav_strictness = so_strictness sopts,
    letToCaseRecord = ltcRec stats,
    propegateRecord = propRec stats }  f mempty smap e where
    smap = Map.fromAscList [ (x,IsBoundTo y) |  (x,y) <- Map.toAscList $ so_boundVars sopts]
    f n (x,xs) = do
        (x',xs') <- doSimplify sopts stats (x,xs) 
        return $ foldl EAp x' xs'
                                  
