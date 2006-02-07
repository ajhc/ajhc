module Grin.FromE(compile,typecheckGrin) where

import Char
import Data.Graph(stronglyConnComp, SCC(..))
import Data.IORef
import Data.Map as Map hiding(map,null)
import Data.Monoid
import List
import Maybe
import qualified Data.Set as Set

import Atom
import Control.Monad.Identity
import C.Prims
import DataConstructors
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.FreeVars
import E.Program
import E.TypeCheck
import E.Values
import GenUtil
import Grin.Grin
import Grin.Show
import Grin.Val
import Name.Name
import Name.Names
import Name.VConsts
import Options
import PrimitiveOperators
import qualified FlagDump as FD
import qualified Stats
import Stats(mtick)
import Support.CanType
import Support.FreeVars
import Util.Graph as G
import Util.Once
import Util.UniqueMonad()



{- | Tags
 'f' - normal function
 'F' - postponed function
 'P' - partial application of function
 'C' - data constructor
 'T' - type constructor
 'Y' - partial application of type constructor (think, broken T)
 'b' - built in funttion
 'B' - postponed built in function (built in functions may not be partially applied)
 '@' - very special function or tag
-}

-------------------
-- Compile E -> Exp
-------------------



data CEnv = CEnv {
    scMap :: Map Int (Atom,[Ty],Ty),
    ccafMap :: Map Int Val,
    tyEnv :: IORef TyEnv,
    funcBaps :: IORef [(Atom,Lam)],
    constMap :: Map Int Val,
    errorOnce :: OnceMap (Ty,String) Atom,
    counter :: IORef Int

}

dumpTyEnv (TyEnv tt) = mapM_ putStrLn $ sort [ fromAtom n <+> hsep (map show as) <+> "::" <+> show t |  (n,(as,t)) <- Map.toList tt]

tagArrow = convertName tc_Arrow


flattenScc xs = concatMap f xs where
    f (AcyclicSCC x) = [x]
    f (CyclicSCC xs) = xs

partialLadder t
    | 'P':cs <- fromAtom t = let
        (n','_':rs) = span isDigit cs
        n = (read n' :: Int)
        in [ toAtom $ 'P':show x ++ "_" ++ rs | x <- [1 .. n]]
    | otherwise = [t]

typecheckGrin grin = do
    let errs = [  (err ++ "\n" ++ render (prettyFun a) ) | (a,Left err) <-  [ (a,typecheck (grinTypeEnv grin) c:: Either String Ty)   | a@(_,(_ :-> c)) <-  grinFunctions grin ]]
    mapM_ putErrLn  errs
    when (not $ null errs) $ fail "There were type errors!"

scTag n = t where (t,_,_) = toEntry (n,undefined,undefined)

cafNum n = V $ - atomIndex (partialTag t 0)
    where
    (t,_,_) = toEntry (n,undefined,undefined)


toEntry (n,as,e)
    | Just nm <- intToAtom (tvrNum n)  = f (toAtom ('f':show (fromAtom nm :: Name)))
    | otherwise = f (toAtom ('f':show (tvrNum n))) where
        f x = (x,map (toType (TyPtr TyNode) . tvrType ) as,toType TyNode (getType (e::E) :: E))

toType :: Ty -> E -> Ty
toType node = toty where
    toty (ELit (LitCons n es ty)) |  ty == eHash, TypeConstructor <- nameType n, Just _ <- fromUnboxedNameTuple n = (TyTup (map (toType (TyPtr TyNode) ) es))
    toty (ELit (LitCons n [] es)) |  es == eHash, RawType <- nameType n = (Ty $ toAtom (show n))
    toty _ = node

compile :: Program -> IO Grin
compile prog@Program { progDataTable = dataTable, progCombinators = cm, progMainEntry = mt } = do
    tyEnv <- newIORef initTyEnv
    funcBaps <- newIORef []
    counter <- newIORef 100000  -- TODO real number
    let (cc,reqcc) = constantCaf prog
    wdump FD.Progress $ do
        putErrLn $ "Found" <+> tshow (length cc) <+> "CAFs to convert to constants," <+> tshow (length reqcc) <+> "of which are recursive."
        putDocMLn putStr $ vcat [ pprint v  | v <- reqcc ]
        putDocMLn putStr $ vcat [ pprint v <+> pprint n <+> pprint e | (v,n,e) <- cc ]
    errorOnce <- newOnceMap
    let doCompile = compile' dataTable CEnv {
            funcBaps = funcBaps,
            tyEnv = tyEnv,
            scMap = scMap,
            counter = counter,
            constMap = mempty,
            errorOnce = errorOnce,
            ccafMap = Map.fromList [ (tvrNum v,e) |(v,_,e) <- cc]
            }
    ds <- mapM doCompile [ c | c@(v,_,_) <- cm, v `notElem` [x | (x,_,_) <- cc]]
    (_,(Tup [] :-> theMain)) <- doCompile ((mt,[],EVar mt))

    wdump FD.Progress $ do
        os <- onceMapToList errorOnce
        mapM_ print os
    let tf a = a:tagToFunction a
    ds <- return $ flattenScc $ stronglyConnComp [ (a,x, concatMap tf (freeVars z)) | a@(x,(_ :-> z)) <- ds]
    TyEnv endTyEnv <- readIORef tyEnv
    let newTyEnv = TyEnv $ Map.fromList $ concatMap makePartials (Map.toList endTyEnv)
    wdump FD.Tags $ do
        dumpTyEnv newTyEnv
    fbaps <- readIORef funcBaps
    --sequence_ [ typecheck te c >>= when False . print . (,) a  | (a,_,c) <-  ds ]
    -- let (main,as,rtype) = runIdentity $ Map.lookup (tvrNum mt) scMap
        -- main' =  if not $ null as then  (Return $ NodeC (partialTag main (length as)) []) else App main [] rtype
        -- tags = Set.toList $ ep $ Set.unions (freeVars (main',initCafs):[ freeVars e | (_,(_ :-> e)) <- ds ])
    let ep s = Set.fromList $ concatMap partialLadder $ Set.toList s
        cafs = [ ((V $ - atomIndex tag),NodeC tag []) | (x,(Tup [] :-> _)) <- ds, let tag = partialTag x 0 ] ++ [ (y,z') |(x,y,z) <- cc, y `elem` reqcc, let Const z' = z ]
        initCafs = sequenceG_ [ Update (Var v (TyPtr TyNode)) node | (v,node) <- cafs ]
        ic = (funcInitCafs,(Tup [] :-> initCafs) )
        ds' = ic:(ds ++ fbaps)
    let grin = emptyGrin {
            grinEntryPoints = [funcMain],
            grinPhase = PhaseInit,
            grinTypeEnv = newTyEnv,
            grinFunctions = (funcMain ,(Tup [] :-> App funcInitCafs [] tyUnit :>>= unit :->  theMain :>>= n0 :-> Return unit )) : ds',
            grinCafs = cafs
            }
    typecheckGrin grin
    return grin
    where
    scMap = fromList [ (tvrNum t,toEntry x) |  x@(t,_,_) <- progCombinators prog]
    initTyEnv = mappend primTyEnv $ TyEnv $ fromList $ [ (a,(b,c)) | (_,(a,b,c)) <-  Map.toList scMap] ++ [con x| x <- Map.elems $ constructorMap dataTable, conType x /= eHash]
    con c | (EPi (TVr { tvrType = a }) b,_) <- fromLam $ conExpr c = (tagArrow,([TyPtr TyNode, TyPtr TyNode],TyNode))
    con c = (n,(as,TyNode)) where
        n | sortStarLike (conType c) = convertName (conName c)
          | otherwise = convertName (conName c)
        as = [ toType (TyPtr TyNode) (getType s) |  s <- conSlots c]


makePartials (fn,(ts,rt)) | tagIsFunction fn, head (show fn) /= '@'  = (fn,(ts,rt)):[(partialTag fn i,(reverse $ drop i $ reverse ts ,TyNode)) |  i <- [0.. end] ]  where
    end | 'b':_ <- show fn = 0
        | otherwise = length ts
makePartials x = [x]

primTyEnv = TyEnv . Map.fromList $ [
    (tagArrow,([TyPtr TyNode, TyPtr TyNode],TyNode)),
    (convertName tc_Absurd, ([],TyNode)),
    (funcInitCafs, ([],tyUnit)),
    (funcEval, ([TyPtr TyNode],TyNode)),
    (funcApply, ([TyNode, TyPtr TyNode],TyNode)),
    (funcMain, ([],tyUnit)),
    (tagHole, ([],TyNode))
    ] ++ [ (toAtom ('C':x), ([Ty $ toAtom y],TyNode)) | (x,y,_) <- allCTypes, y /= "void" ]


-- | constant CAF analysis
-- In grin, partial applications are constant data, rather than functions. Since
-- many cafs consist of constant applications, we preprocess them into values
-- beforehand. This also catches recursive constant toplevel bindings.

constantCaf :: Program -> ([(TVr,Var,Val)],[Var])
constantCaf Program { progDataTable = dataTable, progCombinators = ds } = ans where
    (lbs',cafs) = G.findLoopBreakers (const 0) (const True) $ G.newGraph [ (v,e) | (v,[],e) <- ds, canidate e] (tvrNum . fst) (freeVars . snd)
    lbs = Set.fromList $ fsts lbs'
    canidate (ELit _) = True
    canidate (EPi _ _) = True
    canidate e | (EVar x,as) <- fromAp e, Just vs <- Map.lookup x res, vs > length as = True
    canidate _ = False
    ans = ([ (v,cafNum v,conv e) | (v,e) <- cafs ],[ cafNum v | (v,_) <- cafs, v `Set.member` lbs ])
    res = Map.fromList [ (v,length vs) | (v,vs,_) <- ds]
    coMap = Map.fromList [  (v,ce)| (v,_,ce) <- fst ans]
    conv :: E -> Val
    conv (ELit (LitInt i (ELit (LitCons n [] (ESort EHash))))) | RawType <- nameType n =  Lit i (Ty $ toAtom (show n))
    --conv (ELit (LitInt i (ELit (LitCons n [] (ESort EStar))))) | Just pt <- Map.lookup n ctypeMap = ( Const (NodeC (toAtom $ 'C':show n) [(Lit i (Ty (toAtom pt)))]))
    conv e | Just (a,_) <- from_unsafeCoerce e = conv a
    conv (ELit lc@(LitCons n es _)) | Just nn <- getName lc = (Const (NodeC nn (map conv es)))
    conv (EPi (TVr { tvrIdent = 0, tvrType =  a}) b)  =  Const $ NodeC tagArrow [conv a,conv b]
    conv (EVar v) | v `Set.member` lbs = Var (cafNum v) (TyPtr TyNode)
    conv e | (EVar x,as) <- fromAp e, Just vs <- Map.lookup x res, vs > length as = Const (NodeC (partialTag (scTag x) (vs - length as)) (map conv as))
    conv (EVar v) | Just ce <- Map.lookup v coMap = ce
    conv (EVar v) = Var (cafNum v) (TyPtr TyNode)
    conv x = error $ "conv: " ++ show x
    getName = getName' dataTable

getName' :: (Show a,Monad m) => DataTable -> Lit a E -> m Atom
getName' dataTable v@(LitCons n es _)
    | Just _ <- fromUnboxedNameTuple n = fail $ "unboxed tuples don't have names silly"
    | conAlias cons = error $ "Alias still exists: " ++ show v
    | length es == nargs  = do
        return cn
    | nameType n == TypeConstructor && length es < nargs = do
        return ((partialTag cn (nargs - length es)))
    | otherwise = error $ "Strange name: " ++ show v ++ show nargs ++ show cons
    where
    cn = convertName n
    cons = runIdentity $ getConstructor n dataTable
    nargs = length (conSlots cons)

instance ToVal TVr where
    toVal (TVr { tvrIdent = num, tvrType = (ELit (LitCons n [] es))}) | es == eHash, RawType <- nameType n  = Var (V num) (Ty $ toAtom (show n))
    toVal tvr = Var  (V (tvrNum tvr)) (TyPtr TyNode) -- (toTy $ tvrType tvr)

-- constraints during compilation:
--
-- ce always produces something of type TyNode
-- cc always produces something of type (TyPtr TyNode)
-- all functions are assumed to return a TyNode
-- This is no longer true.
--
-- right after compilation, everything has type TyNode or (TyPtr TyNode) since
-- Haskell does not natively support anything other than boxed types.
-- This also isn't true.
--

dropCoerce e | Just (x,_) <- from_unsafeCoerce e = x
dropCoerce x = x

compile' ::  DataTable -> CEnv -> (TVr,[TVr],E) -> IO (Atom,Lam)
compile' dataTable cenv (tvr,as,e) = ans where
    ans = do
        --putStrLn $ "Compiling: " ++ show nn
        x <- cr e
        return (nn,(Tup (map toVal as) :-> x))
    funcName = maybe (show $ tvrNum tvr) show (fmap fromAtom ( intToAtom $ tvrNum tvr) :: Maybe Name)
    cc, ce, cr :: E -> IO Exp
    (nn,_,_) = runIdentity $ Map.lookup (tvrNum tvr) (scMap cenv)
    cr x = ce x

    -- | ce evaluates something in strict context returning the evaluated result of its argument.
    ce (ELetRec ds e) = ce e >>= \e -> doLet ds e
    ce (EError s e) = return (Error s (toType TyNode e))
    ce (EVar tvr@(TVr { tvrType = (ELit (LitCons n [] _))})) | RawType <- nameType n = do
        return (Return (toVal tvr))
    ce e |  (v,as) <- fromAp e, EVar v <- dropCoerce v = do
        as <- return $ args as
        let fty = toType TyNode (getType e)
        case Map.lookup (tvrNum v) (scMap cenv) of
            Just (_,[],_) -> do
                case constant (EVar v) of
                    Just (Const x) -> app fty (Return x) as
                    Just x@Var {} -> app fty (gEval x) as
            Just (v,as',es)
                | length as >= length as' -> do
                    let (x,y) = splitAt (length as') as
                    app fty (App v x es) y
                | otherwise -> do
                    let pt = partialTag v (length as' - length as)
                    return $ Return (NodeC pt as)
            Nothing -> app fty (gEval $ toVal v) as
    ce e | (v,as@(_:_)) <- fromAp e = do
        let fty = toType TyNode (getType e)
        as <- return $ args as
        e <- ce v
        app fty e as
    ce (EPi (TVr { tvrIdent = 0, tvrType = a}) b) = do
        a' <- cc a
        b' <- cc b
        p1 <- newNodePtrVar
        p2 <- newNodePtrVar
        return (a' :>>= p1 :-> b' :>>= p2 :-> Return (NodeC tagArrow [p1,p2]))
    ce e | Just z <- literal e = return (Return z)
    ce e | Just (Const z) <- constant e = return (Return z)
    ce e | Just z <- constant e = return (gEval z)
    ce e | Just z <- con e = return (Return z)
    ce e | Just (a,_) <- from_unsafeCoerce e = ce a
    ce (EPrim ap@(APrim (Func True fn as "void") _) (_:es) _) = do
        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = ((map (Ty . toAtom) as),tyUnit), primAPrim = ap }
        return $  Prim p (args es) :>>= unit :-> Return world__
    ce (EPrim ap@(APrim (Func True fn as r) _) (_:es) rt) = do
        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = ((map (Ty . toAtom) as),Ty (toAtom r)), primAPrim = ap }
            ptv = Var v2 pt
            pt = Ty (toAtom r)
        return $ Prim p (args es) :>>= ptv :-> Return (Tup [pworld__,ptv])
    ce (EPrim ap@(APrim (Func False _ as r) _) es (ELit (LitCons tname [] _))) | RawType <- nameType tname = do
        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = ((map (Ty . toAtom) as),Ty (toAtom r)), primAPrim = ap }
        return $ Prim p (args es)
    ce (EPrim ap@(APrim (Peek pt') _) [_,addr] rt) = do
        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = ([Ty (toAtom "HsPtr")],pt), primAPrim = ap }
            ptv = Var v2 pt
            pt = Ty (toAtom pt')
        return $  Prim p (args [addr]) :>>= ptv :-> Return (Tup [pworld__,ptv])
    ce (EPrim ap@(APrim (Poke pt') _) [_,addr,val] _) = do
        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = ([Ty (toAtom "HsPtr"),pt],tyUnit), primAPrim = ap }
            pt = Ty (toAtom pt')
        return $  Prim p (args [addr,val]) :>>= unit :-> Return world__
    ce (EPrim aprim@(APrim (AddrOf s) _) [] (ELit (LitCons tname [] _))) | RawType <- nameType tname = do
        let p = Primitive { primName = toAtom ('&':s), primRets = Nothing, primType = ([],ptype), primAPrim = aprim }
            ptype = Ty $ toAtom (show tname)
        return $ Prim p []
    ce (EPrim aprim@(APrim (CConst s t) _) [] (ELit (LitCons n [] _))) | RawType <- nameType n = do
        let p = Primitive { primName = toAtom s, primRets = Nothing, primType = ([],ptype), primAPrim = aprim }
            ptype = Ty $ toAtom t
        return $ Prim p []
    ce ee@(EPrim aprim@(APrim (CCast from to) _) [e] t)  = do
        let ptypeto' = Ty $ toAtom to
            ptypefrom' = Ty $ toAtom from
        let p = Primitive { primName = toAtom ("(" ++ to ++ ")"), primRets = Nothing, primType = ([ptypefrom'],ptypeto'), primAPrim = aprim }
        return $  Prim p (args [e])
    ce (EPrim ap@(APrim (Operator n as r) _) es (ELit (LitCons tname [] _))) | RawType <- nameType tname = do
        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = ((map (Ty . toAtom) as),Ty (toAtom r)), primAPrim = ap }
        return $ Prim p (args es)
    ce (ECase e _ [Alt (LitCons n xs _) wh] _) | Just _ <- fromUnboxedNameTuple n, DataConstructor <- nameType n  = do
        e <- ce e
        wh <- ce wh
        return $ e :>>= Tup (map toVal xs) :-> wh

    ce (ECase e b as d) | (ELit (LitCons n [] _)) <- getType e, RawType <- nameType n = do
            let ty = Ty $ toAtom (show n)
            v <- newPrimVar ty
            e <- ce e
            as' <- mapM cp'' as
            def <- createDef d (return (toVal b))
            return $
                e :>>= v :-> Case v (as' ++ def)
    ce (ECase e b as d)  = do
        v <- newNodeVar
        e <- ce e
        as <- mapM cp as
        def <- createDef d (newNodeVar)
        return $
            e :>>= v :->
            Store v :>>= toVal b :->
            Case v (as ++ def)
    ce e = error $ "ce: " ++ show (funcName,e)
    fromIORT e | ELit (LitCons tn [x,y] star) <- followAliases dataTable e, tn == tupNamet2, star == eStar, x == tWorld__ = lookupCType dataTable y
    fromIORT e = fail $ "fromIORT: " ++ show e
    retIO v = Return (NodeC tn_2Tup [pworld__,v])
    tupNamet2 = (nameTuple TypeConstructor 2)

    createDef Nothing _ = return []
    createDef (Just e) nnv = do
        nv <- nnv
        x <- ce e
        return [nv :-> x]
    cp (Alt lc@(LitCons n es _) e) | Just v <- fromUnboxedNameTuple n, DataConstructor <- nameType n = do
        putStrLn $ "Print alt: " ++ show lc
        x <- ce e
        return (Tup (map toVal es) :-> x)
    cp (Alt lc@(LitCons n es _) e) = do
        x <- ce e
        nn <- getName lc
        return (NodeC nn (map toVal es) :-> x)
    cp x = error $ "cp: " ++ show (funcName,x)
    cp'' (Alt (LitInt i (ELit (LitCons nn [] _))) e) = do
        x <- ce e
        return (Lit i (Ty $ toAtom (show nn)) :-> x)

    getName x = getName' dataTable x

    app _ e [] = return e
    app ty e [a] = do
        v <- newNodeVar
        return (e :>>= v :-> App funcApply [v,a] ty)
    app ty e (a:as) = do
        v <- newNodeVar
        app ty (e :>>= v :-> gApply v a) as
    app' e [] = return $ Return e
    app' (Const (NodeC t cs)) (a:as) | tagIsPartialAp t = do
        let ('P':rs') = fromAtom t
            (n','_':rs) = span isDigit rs'
            n = (read n' :: Int)
            lazy = do
                mtick "Grin.FromE.lazy-app-const"
                app' (Const (NodeC (partialTag (toAtom $ 'f':rs) (n - 1)) (cs ++ [a]))) as
        case a of
            Const {} -> lazy
            Lit {} -> lazy
            Var (V n) _ | n < 0 -> lazy
            _ -> do
                mtick "Grin.FromE.lazy-app-store"
                tpv <- newNodePtrVar
                x <- app' tpv as
                return $ Store  (NodeC (partialTag (toAtom $ 'f':rs) (n - 1)) (cs ++ [a])) :>>= tpv :-> x
    app' e as = do
        mtick "Grin.FromE.lazy-app-bap"
        V vn <- newVar
        let t  = toAtom $ "Bap_" ++ show (length as) ++ "_" ++ funcName ++ "_" ++ show vn
            tl = toAtom $ "bap_" ++ show (length as) ++ "_" ++  funcName ++ "_" ++ show vn
            args = [Var v (TyPtr TyNode) | v <- [v1..] | _ <- (undefined:as)]
            s = Store (NodeC t (e:as))
        d <- app TyNode (gEval p1) (tail args) --TODO
        addNewFunction (tl,Tup (args) :-> d)
        return s
    addNewFunction tl@(n,Tup args :-> body) = do
        modifyIORef (funcBaps cenv) (tl:)
        let addt (TyEnv mp) =  TyEnv $ Map.insert n (map getType args,getType body) mp
        modifyIORef (tyEnv cenv) addt

    -- | cc evaluates something in lazy context, returning a pointer to a node which when evaluated will produce the strict result.
    -- it is an invarient that evaling (cc e) produces the same value as (ce e)
    cc e | Just _ <- literal e = error "literal in lazy context"
    cc e | Just z <- constant e = return (Return z)
    cc e | Just z <- con e = return (Store z)
    cc (EError s e) = do
        let ty = toType TyNode e
        a <- runOnceMap (errorOnce cenv) (ty,s) $ do
            u <- newUniq
            let t  = toAtom $ "Berr_" ++ show u
                tl = toAtom $ "berr_" ++ show u
            addNewFunction (tl,Tup [] :-> Error s ty)
            return t
        return $ Return (Const (NodeC a []))
    cc (ELetRec ds e) = cc e >>= \e -> doLet ds e
    cc e |  (v,as) <- fromAp e, EVar v <- dropCoerce v = do
        as <- return $ args as
        case Map.lookup (tvrNum v) (scMap cenv) of
            Just (_,[],_) | Just x <- constant (EVar v) -> app' x as
            Just (v,as',es)
                | length as > length as' -> do
                    let (x,y) = splitAt (length as') as
                    let s = Store (NodeC (partialTag v 0) x)
                    nv <- newNodePtrVar
                    z <- app' nv y
                    return $ s :>>= nv :-> z
                | otherwise -> do
                    let pt = partialTag v (length as' - length as)
                    return $ Store (NodeC pt as)
            Nothing
                | [] <- as -> return $ Return (toVal v)
                | otherwise  -> app' (toVal v) as
    cc e | Just (x,_) <- from_unsafeCoerce e = cc x
    cc (EPrim aprim@(APrim prim _) es pt) = do
        V vn <- newVar
        te <- readIORef (tyEnv cenv)
        let s = pprint prim
            fn' = toAtom ('B':s ++ "_" ++ show vn)
            fn = toAtom ('b':s ++ "_" ++ show vn)
        case findArgsType te fn of
            Just _ -> return $ Store $ NodeC fn' (args es)
            Nothing -> do
                let es' = args es
                ts <- mapM (typecheck te) es'
                let nvs = [ Var v t | t <- ts | v <- [v2,V 4..] ]
                x <- ce (EPrim aprim [ EVar (tvr { tvrIdent = v, tvrType =  t}) | t <- map getType es | v <- [2,4..]] pt)
                addNewFunction (fn,Tup nvs :-> x)
                return $ Store $ NodeC fn' es'
    cc e = error $ "cc: " ++ show e
    doLet ds e = f (decomposeDefns ds) e where
        f [] x = return x
        f (Left (t,e):ds) x = do
            e <- cc e
            v <- f ds x
            return $ e :>>= toVal t :-> v
        f (Right bs:ds) x = do
            let g (tvr,_) y = (Store (NodeC (toAtom "@hole") []) :>>= toVal tvr :-> y)
                u (tvr,e) = do
                    v <- newNodePtrVar
                    v' <- newNodeVar
                    e <- cc e
                    return $ doUpdate (toVal tvr) e
            xs <- mapM u bs
            v <- f ds x
            let r = (foldr (\a b -> a :>>= unit :-> b) v xs)
            return $ foldr g r bs

    -- This avoids a blind update on recursive thunks
    doUpdate vr (Store n) = Update vr n
    doUpdate vr (x :>>= v :-> e) = x :>>= v :-> doUpdate vr e
    doUpdate vr x = error $ "doUpdate: " ++ show x
    args es = map f es where
        f x | Just z <- literal x = z
        f x | Just z <- constant x =  z
        f (EVar tvr) = toVal tvr
        f e | Just (x,_) <- from_unsafeCoerce e = f x
        f x = error $ "invalid argument: " ++ show x

    -- | converts an unboxed literal
    literal :: Monad m =>  E -> m Val
    literal (ELit (LitInt i (ELit (LitCons n [] (ESort EHash))))) | RawType <- nameType n = return $ Lit i (Ty $ toAtom (show n))
    literal _ = fail "not a literal term"


    -- | Takes an E and returns something constant which is either a pointer to
    -- a constant heap location only pointing to global values or constants.
    -- this includes a CAF which may be evaluated, a literal, a saturated
    -- application of constant values to a supercombinator, or a constructor
    -- containing constant values. constant is sort of a misnomer here when
    -- runtime behavior is considered, it means a compile time constant, the
    -- CAFs may be updated with evaluated values.

    constant :: Monad m =>  E -> m Val
    constant (EVar tvr) | Just c <- Map.lookup (tvrNum tvr) (ccafMap cenv) = return c
                        | Just (v,as,_) <- Map.lookup (tvrNum tvr) (scMap cenv)
                         , t <- partialTag v (length as)  = case tagIsWHNF t of
                            True -> return $ Const $ NodeC t []
                            False -> return $ Var (V $ - atomIndex t) (TyPtr TyNode)
                            --case constant e of
                            --    Just x -> return x
                            --    Nothing -> return $ Var (V $ - atomIndex t) (TyPtr TyNode)
    constant e | Just l <- literal e = return l
    constant (ELit lc@(LitCons n es _)) | Just es <- mapM constant es, Just nn <- getName lc = (return $ Const (NodeC nn es))
    constant (EPi (TVr { tvrIdent = 0, tvrType = a}) b) | Just a <- constant a, Just b <- constant b = return $ Const $ NodeC tagArrow [a,b]
    constant e | Just (a,_) <- from_unsafeCoerce e = constant a
    constant _ = fail "not a constant term"

    -- | convert a constructor into a Val, arguments may depend on local vars.
    con :: Monad m => E -> m Val
    con (EPi (TVr {tvrIdent =  0, tvrType = x}) y) = do
        return $  NodeC tagArrow (args [x,y])
    con v@(ELit (LitCons n es _))
        | conAlias cons = error $ "Alias still exists: " ++ show v
        | Just v <- fromUnboxedNameTuple n, DataConstructor <- nameType n = do
            return (Tup (args es))
        | length es == nargs  = do
            return (NodeC cn (args es))
        | nameType n == TypeConstructor && length es < nargs = do
            return (NodeC (partialTag cn (nargs - length es)) $ args es)
        where
        cn = convertName n
        cons = runIdentity $ getConstructor n dataTable
        nargs = length (conSlots cons)

    con _ = fail "not constructor"


    scInfo tvr | Just n <- Map.lookup (tvrNum tvr) (scMap cenv) = return n
    scInfo tvr = fail $ "not a supercombinator:" <+> show tvr
    newNodeVar =  fmap (\x -> Var x TyNode) newVar
    newPrimVar ty =  fmap (\x -> Var x ty) newVar
    newNodePtrVar =  fmap (\x -> Var x (TyPtr TyNode)) newVar
    newVar = do
        i <- readIORef (counter cenv)
        writeIORef (counter cenv) $! (i + 2)
        return $! V i

