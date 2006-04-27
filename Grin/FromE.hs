module Grin.FromE(compile,typecheckGrin) where

import Char
import Data.Graph(stronglyConnComp, SCC(..))
import Data.IORef
import Data.Typeable
import qualified Data.Map as Map
import Data.Map(Map)
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
import qualified Info.Info as Info
import Info.Types
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
import Support.Tuple
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

-- after pruning the E, we should not accidentally turn things into CAFs that wern't already.
newtype IsCAF = IsCAF Bool
    deriving(Typeable,Show,Eq)

dumpTyEnv (TyEnv tt) = mapM_ putStrLn $ sort [ fromAtom n <+> hsep (map show as) <+> "::" <+> show t |  (n,(as,t)) <- Map.toList tt]

tagArrow = convertName tc_Arrow
tagRef = toAtom "CData.IORef.Ref"


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
    unless (null errs || optKeepGoing options) $ fail "There were type errors!"

scTag n = t where (t,_,_) = toEntry (n,undefined,undefined)

cafNum n = V $ - atomIndex (partialTag t 0)
    where
    (t,_,_) = toEntry (n,undefined,undefined)


toEntry (n,as,e)
    | Just nm <- intToAtom (tvrIdent n)  = f (toAtom ('f':show (fromAtom nm :: Name)))
    | otherwise = f (toAtom ('f':show (tvrIdent n))) where
        f x = (x,map (toType (TyPtr TyNode) . tvrType ) $ filter (shouldKeep . getType )as,toType TyNode (getType (e::E) :: E))

toType :: Ty -> E -> Ty
toType node = toty where
    toty e | e == tWorld__ = TyTup []
    toty (ELit (LitCons n es ty)) |  ty == eHash, TypeConstructor <- nameType n, Just _ <- fromUnboxedNameTuple n = (tuple (map (toType (TyPtr TyNode) ) (filter shouldKeep es)))
    toty (ELit (LitCons n [] es)) |  es == eHash, RawType <- nameType n = (Ty $ toAtom (show n))
    toty _ = node

compile :: Program -> IO Grin
compile prog@Program { progDataTable = dataTable, progMainEntry = mainEntry } = do
    prog <- return $ prog { progCombinators  = map stripTheWorld (progCombinators prog) }
    tyEnv <- newIORef initTyEnv
    funcBaps <- newIORef []
    counter <- newIORef 100000  -- TODO real number
    let (cc,reqcc,rcafs) = constantCaf prog
    wdump FD.Progress $ do
        putErrLn $ "Found" <+> tshow (length cc) <+> "CAFs to convert to constants," <+> tshow (length reqcc) <+> "of which are recursive."
        putErrLn "Recursive"
        putDocMLn putStr $ vcat [ pprint v  | v <- reqcc ]
        putErrLn "Constant"
        putDocMLn putStr $ vcat [ pprint v <+> pprint n <+> pprint e | (v,n,e) <- cc ]
        putErrLn "CAFS"
        putDocMLn putStr $ vcat [ pprint v <+> pprint n <+> pprint e | (v,n,e) <- rcafs ]
    errorOnce <- newOnceMap
    let doCompile = compile' dataTable CEnv {
            funcBaps = funcBaps,
            tyEnv = tyEnv,
            scMap = scMap,
            counter = counter,
            constMap = mempty,
            errorOnce = errorOnce,
            ccafMap = Map.fromList $ [(tvrIdent v,e) |(v,_,e) <- cc ]  ++ [ (tvrIdent v,Var vv (TyPtr TyNode)) | (v,vv,_) <- rcafs]
            }
    ds <- mapM doCompile [ c | c@(v,_,_) <- progCombinators prog, v `notElem` [x | (x,_,_) <- cc]]
    --(_,(Tup [] :-> theMain)) <- doCompile ((mainEntry,[],EVar mainEntry))
    let theMain = App (scTag mainEntry) [] tyUnit

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
    -- let (main,as,rtype) = runIdentity $ Map.lookup (tvrIdent mt) scMap
        -- main' =  if not $ null as then  (Return $ NodeC (partialTag main (length as)) []) else App main [] rtype
        -- tags = Set.toList $ ep $ Set.unions (freeVars (main',initCafs):[ freeVars e | (_,(_ :-> e)) <- ds ])
    let ep s = Set.fromList $ concatMap partialLadder $ Set.toList s
        -- cafs = [ ((V $ - atomIndex tag),NodeC tag []) | (x,(Tup [] :-> _)) <- ds, let tag = partialTag x 0 ] ++ [ (y,z') |(x,y,z) <- cc, y `elem` reqcc, let Const z' = z ]
        cafs = [ (x,y) | (_,x,y) <- rcafs ]
        initCafs = sequenceG_ [ Update (Var v (TyPtr TyNode)) node | (v,node) <- cafs ]
        ic = (funcInitCafs,(Tup [] :-> initCafs) )
        ds' = ic:(ds ++ fbaps)
    let grin = emptyGrin {
            grinEntryPoints = [funcMain],
            grinPhase = PhaseInit,
            grinTypeEnv = newTyEnv,
            grinFunctions = (funcMain ,(Tup [] :-> App funcInitCafs [] tyUnit :>>= unit :->  discardResult theMain)) : ds',
            grinCafs = [ (x,NodeC tagHole []) | (x,_) <- cafs]
            }
    --typecheckGrin grin
    return grin
    where
    scMap = Map.fromList [ (tvrIdent t,toEntry x) |  x@(t,_,_) <- map stripTheWorld $ progCombinators prog]
    initTyEnv = mappend primTyEnv $ TyEnv $ Map.fromList $ [ (a,(b,c)) | (_,(a,b,c)) <-  Map.toList scMap] ++ [con x| x <- Map.elems $ constructorMap dataTable, conType x /= eHash]
    con c | (EPi (TVr { tvrType = a }) b,_) <- fromLam $ conExpr c = (tagArrow,([TyPtr TyNode, TyPtr TyNode],TyNode))
    con c = (n,(as,TyNode)) where
        n | sortStarLike (conType c) = convertName (conName c)
          | otherwise = convertName (conName c)
        as = [ toType (TyPtr TyNode) (getType s) |  s <- conSlots c, shouldKeep s]

discardResult exp = case getType exp of
    TyTup [] -> exp
    t -> exp :>>= et t :-> Return unit
    where
    et (TyTup xs) = Tup (map et xs)
    et t = Var v0 t

stripTheWorld :: (TVr,[TVr],E) ->  (TVr,[TVr],E)
stripTheWorld (t,as,e) = (tvrInfo_u (Info.insert (IsCAF caf)) t,filter (shouldKeep . getType) as,e) where
    caf = null as


shouldKeep :: E -> Bool
shouldKeep e = e /= tWorld__


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
    ] ++ [ (toAtom ('C':show dc), ([Ty $ toAtom y],TyNode)) | (dc,tc,_,y,_) <- allCTypes, y /= "void" ]


-- | constant CAF analysis
-- In grin, partial applications are constant data, rather than functions. Since
-- many cafs consist of constant applications, we preprocess them into values
-- beforehand. This also catches recursive constant toplevel bindings.

constantCaf :: Program -> ([(TVr,Var,Val)],[Var],[(TVr,Var,Val)])
constantCaf Program { progDataTable = dataTable, progCombinators = ds } = ans where
    -- All CAFS
    ecafs = [ (v,e) | (v,[],e) <- ds, Just (IsCAF True) == Info.lookup (tvrInfo v) ]
    -- just CAFS that can be converted to constants need dependency analysis
    (lbs',cafs) = G.findLoopBreakers (const 0) (const True) $ G.newGraph (filter (canidate . snd) ecafs) (tvrIdent . fst) (freeVars . snd)
    lbs = Set.fromList $ fsts lbs'
    canidate (ELit _) = True
    canidate (EPi _ _) = True
    canidate e | (EVar x,as) <- fromAp e, Just vs <- Map.lookup x res, vs > length (ff as) = True
    canidate _ = False
    ans = ([ (v,cafNum v,conv e) | (v,e) <- cafs ],[ cafNum v | (v,_) <- cafs, v `Set.member` lbs ], [(v,cafNum v, NodeC (partialTag n 0) []) | (v,e) <- ecafs, not (canidate e), let n = scTag v ])
    res = Map.fromList [ (v,length $ ff vs) | (v,vs,_) <- ds]
    coMap = Map.fromList [  (v,ce)| (v,_,ce) <- fst3 ans]
    conv :: E -> Val
    conv e | Just v <- literal e = v
    conv (ELit lc@(LitCons n es _)) | Just nn <- getName lc = (Const (NodeC nn (map conv es)))
    conv (EPi (TVr { tvrIdent = 0, tvrType =  a}) b)  =  Const $ NodeC tagArrow [conv a,conv b]
    conv (EVar v) | v `Set.member` lbs = Var (cafNum v) (TyPtr TyNode)
    conv e | (EVar x,as) <- fromAp e, Just vs <- Map.lookup x res, vs > length (ff as) = Const (NodeC (partialTag (scTag x) (vs - length (ff as))) (map conv (ff as)))
    conv (EVar v) | Just ce <- Map.lookup v coMap = ce
    conv (EVar v) = Var (cafNum v) (TyPtr TyNode)
    conv x = error $ "conv: " ++ show x
    getName = getName' dataTable
    ff x = filter (shouldKeep . getType) x
    fst3 (x,_,_) = x

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
    toVal (TVr { tvrIdent = num, tvrType = w}) | w == tWorld__ = Tup []-- Var v0 tyUnit -- es == eHash, RawType <- nameType n  = Var (V num) (Ty $ toAtom (show n))
    toVal (TVr { tvrIdent = num, tvrType = (ELit (LitCons n [] es))}) | es == eHash, RawType <- nameType n  = Var (V num) (Ty $ toAtom (show n))
    toVal tvr = Var  (V (tvrIdent tvr)) (TyPtr TyNode) -- (toTy $ tvrType tvr)

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


evalVar tvr | Just CaseDefault <- Info.lookup (tvrInfo tvr)  = do
        mtick "Grin.FromE.strict-casedefault"
        return (Fetch (toVal tvr))
evalVar tvr | getProperty prop_WHNF tvr = do
        mtick "Grin.FromE.strict-propevaled"
        return (Fetch (toVal tvr))
evalVar tvr = return $ gEval (toVal tvr)

compile' ::  DataTable -> CEnv -> (TVr,[TVr],E) -> IO (Atom,Lam)
compile' dataTable cenv (tvr,as,e) = ans where
    ans = do
        --putStrLn $ "Compiling: " ++ show nn
        x <- cr e
        return (nn,(Tup (map toVal (filter (shouldKeep . getType) as)) :-> x))
    funcName = maybe (show $ tvrIdent tvr) show (fmap fromAtom ( intToAtom $ tvrIdent tvr) :: Maybe Name)
    cc, ce, cr :: E -> IO Exp
    (nn,_,_) = runIdentity $ Map.lookup (tvrIdent tvr) (scMap cenv)
    cr x = ce x

    -- | ce evaluates something in strict context returning the evaluated result of its argument.
    ce (ELetRec ds e) = ce e >>= \e -> doLet ds e
    ce (EError s e) = return (Error s (toType TyNode e))
    ce (EVar tvr) | isUnboxed (getType tvr) = do
        return (Return (toVal tvr))
    ce (EVar tvr) | not $ isLifted (EVar tvr)  = do
        mtick "Grin.FromE.strict-unlifted"
        return (Fetch (toVal tvr))
    ce e | (EVar tvr,as) <- fromAp e = do
        as <- return $ args as
        let fty = toType TyNode (getType e)
        case Map.lookup (tvrIdent tvr) (ccafMap cenv) of
            Just (Const c) -> app fty (Return c) as
            Just x@Var {} -> app fty (gEval x) as
            Nothing -> case Map.lookup (tvrIdent tvr) (scMap cenv) of
                Just (v,as',es)
                    | length as >= length as' -> do
                        let (x,y) = splitAt (length as') as
                        app fty (App v x es) y
                    | otherwise -> do
                        let pt = partialTag v (length as' - length as)
                        return $ Return (NodeC pt as)
                Nothing | not (isLifted $ EVar tvr) -> do
                    mtick "Grin.FromE.app-unlifted"
                    app fty (Fetch $ toVal tvr) as
                Nothing -> do
                    ee <- evalVar tvr
                    app fty ee as
    ce e | Just z <- literal e = return (Return z)
    ce e | Just (Const z) <- constant e = return (Return z)
    ce e | Just z <- constant e = return (gEval z)
    ce e | Just z <- con e = return (Return z)
    ce (EPrim ap@(APrim (PrimPrim "newHole_") _) [_] _) = do
        let var = Var v2 (TyPtr TyNode)
        return $ Store (NodeC (toAtom "@hole") []) :>>= var :-> Return (tuple [var])
    ce (EPrim ap@(APrim (PrimPrim "fillHole__") _) [r,v,_] _) = do
        let var = Var v2 TyNode
            [r',v'] = args [r,v]
        return $ gEval v' :>>= n1 :-> Update r' n1
    ce (EPrim ap@(APrim (PrimPrim "writeRef__") _) [r,v,_] _) = do
        let var = Var v2 TyNode
            [r',v'] = args [r,v]
        return $ Update r' (NodeC (toAtom "CData.IORef.IORef") [v'])
    ce (EPrim ap@(APrim (PrimPrim "newWorld__") _) [_] _) = do
        return $ Return unit
    ce (EPrim ap@(APrim (PrimPrim "drop__") _) [_,e] _) = ce e
    ce (EPrim ap@(APrim p _) xs ty) = let
      prim = Primitive { primName = Atom.fromString (pprint ap), primAPrim = ap, primRets = Nothing, primType = ([],tyUnit) }
      in case p of
        Func True fn as "void" -> return $ Prim prim { primType = ((map (Ty . toAtom) as),tyUnit) } (args $ tail xs)
        Func True fn as r -> do
            let p = prim { primType = ((map (Ty . toAtom) as),Ty (toAtom r)) }
                ptv = Var v2 pt
                pt = Ty (toAtom r)
            return $ Prim p (args $ tail xs)
        Func False _ as r | Just _ <- fromRawType ty ->  do
            let p = prim { primType = ((map (Ty . toAtom) as),Ty (toAtom r)) }
            return $ Prim p (args xs)
        Peek pt' -> do
            let p = prim { primType = ([Ty $ toAtom (show rt_HsPtr)],pt) }
                [_,addr] = xs
                ptv = Var v2 pt
                pt = Ty (toAtom pt')
            return $ Prim p (args [addr])
        Poke pt' ->  do
            let p = prim { primType = ([Ty $ toAtom (show rt_HsPtr)],tyUnit) }
                [_,addr,val] = xs
                pt = Ty (toAtom pt')
            return $  Prim p (args [addr,val])
        CCast from to -> do
            let ptypeto' = Ty $ toAtom to
                ptypefrom' = Ty $ toAtom from
            let p = prim { primName = toAtom ("(" ++ to ++ ")"), primType = ([ptypefrom'],ptypeto') }
            return $  Prim p (args xs)
        Operator n as r | Just _ <- fromRawType ty -> do
            let p = prim { primType = ((map (Ty . toAtom) as),Ty (toAtom r)) }
            return $ Prim p (args xs)
        other -> fail $ "ce unknown primitive: " ++ show other
    ce ECase { eCaseScrutinee = e, eCaseAlts = [Alt (LitCons n xs _) wh] } | Just _ <- fromUnboxedNameTuple n, DataConstructor <- nameType n  = do
        e <- ce e
        wh <- ce wh
        return $ e :>>= tuple (map toVal (filter (shouldKeep . getType) xs)) :-> wh
    ce ECase { eCaseScrutinee = e, eCaseAlts = [], eCaseDefault = (Just r)} | getType e == tWorld__ = do
        e <- ce e
        r <- ce r
        return $ e :>>= unit :-> r
    ce ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault = d } | (ELit (LitCons n [] _)) <- getType e, RawType <- nameType n = do
            let ty = Ty $ toAtom (show n)
            v <- newPrimVar ty
            e <- ce e
            as' <- mapM cp'' as
            def <- createDef d (return (toVal b))
            return $
                e :>>= v :-> Return v :>>= toVal b :-> Case v (as' ++ def)
    ce ECase { eCaseScrutinee = scrut, eCaseBind = b, eCaseAlts = as, eCaseDefault = d }  = do
        v <- newNodeVar
        e <- ce scrut
        as <- mapM cp as
        def <- createDef d newNodeVar
        return $ case (def,b,scrut) of
            --([],_,_) -> e :>>= v :-> Case v as
            (_,TVr {tvrIdent = 0 },_) -> e :>>= v :-> Case v (as ++ def)
            (_,_,EVar etvr) ->  e :>>= v :-> Return (toVal etvr) :>>= toVal b :-> Case v (as ++ def)
            (_,_,_) -> e :>>= v :-> Store v :>>= toVal b :-> Case v (as ++ def)
    ce e = error $ "ce: " ++ render (pprint (funcName,e))


    createDef Nothing _ = return []
    createDef (Just e) nnv = do
        nv <- nnv
        x <- ce e
        return [nv :-> x]
    cp (Alt lc@(LitCons n es _) e) = do
        x <- ce e
        nn <- getName lc
        return (NodeC nn (map toVal (filter (shouldKeep . getType) es)) :-> x)
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
            args = [Var v ty | v <- [v1..] | ty <- (TyPtr TyNode:map getType as)]
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
    cc (EPrim (APrim (PrimPrim "drop__") _) [_,e] _) = cc e
    cc e | Just _ <- literal e = error "unboxed literal in lazy context"
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
    cc e | (EVar v,as@(_:_)) <- fromAp e = do
        as <- return $ args as
        case Map.lookup (tvrIdent v) (scMap cenv) of
            Just (_,[],_) | Just x <- constant (EVar v) -> app' x as
            Just (v,as',es)
                | length as > length as' -> do
                    let (x,y) = splitAt (length as') as
                    let s = Store (NodeC (partialTag v 0) x)
                    nv <- newNodePtrVar
                    z <- app' nv y
                    return $ s :>>= nv :-> z
                | length as < length as', all valIsConstant as -> do
                    let pt = partialTag v (length as' - length as)
                    mtick "Grin.FromE.partial-constant"
                    return $ Return (Const (NodeC pt as))
                | length as < length as' -> do
                    let pt = partialTag v (length as' - length as)
                    return $ if all valIsConstant as then
                        Return (Const (NodeC pt as))
                            else Store (NodeC pt as)
                | otherwise -> do -- length as == length as'
                    return $ Store (NodeC (tagFlipFunction v) as)
            Nothing -> app' (toVal v) as
    cc (EVar v) = do
        return $ Return (toVal v)
    cc e = return $ error ("cc: " ++ show e)


    doLet ds e = f (decomposeDefns ds) e where
        f [] x = return x
        f (Left (t,e):ds) x | not (isLifted (EVar t)) = do
            mtick "Grin.FromE.let-unlifted"
            e <- ce e
            v <- f ds x
            return $ (e :>>= n1 :-> Store n1) :>>= toVal t :-> v
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
    args es = map f (filter (shouldKeep . getType) es) where
        f x | Just z <- literal x = z
        f x | Just z <- constant x =  z
        f (EVar tvr) = toVal tvr
        f x = error $ "invalid argument: " ++ show x



    -- | Takes an E and returns something constant which is either a pointer to
    -- a constant heap location only pointing to global values or constants.
    -- this includes a CAF which may be evaluated, a literal, a saturated
    -- application of constant values to a supercombinator, or a constructor
    -- containing constant values. constant is sort of a misnomer here when
    -- runtime behavior is considered, it means a compile time constant, the
    -- CAFs may be updated with evaluated values.

    constant :: Monad m =>  E -> m Val
    constant (EVar tvr) | Just c <- Map.lookup (tvrIdent tvr) (ccafMap cenv) = return c
                        | Just (v,as,_) <- Map.lookup (tvrIdent tvr) (scMap cenv)
                         , t <- partialTag v (length as), tagIsWHNF t = return $ Const $ NodeC t []
    --                        False -> return $ Var (V $ - atomIndex t) (TyPtr TyNode)
    constant e | Just l <- literal e = return l
    constant (ELit lc@(LitCons n es _)) | Just es <- mapM constant (filter (shouldKeep . getType) es), Just nn <- getName lc = (return $ Const (NodeC nn es))
    constant (EPi (TVr { tvrIdent = 0, tvrType = a}) b) | Just a <- constant a, Just b <- constant b = return $ Const $ NodeC tagArrow [a,b]
    constant _ = fail "not a constant term"

    -- | convert a constructor into a Val, arguments may depend on local vars.
    con :: Monad m => E -> m Val
    con (EPi (TVr {tvrIdent =  0, tvrType = x}) y) = do
        return $  NodeC tagArrow (args [x,y])
    con v@(ELit (LitCons n es _))
        | conAlias cons = error $ "Alias still exists: " ++ show v
        | Just v <- fromUnboxedNameTuple n, DataConstructor <- nameType n = do
            return (tuple (args (filter (shouldKeep . getType) es)))
        | length es == nargs  = do
            return (NodeC cn (args es))
        | nameType n == TypeConstructor && length es < nargs = do
            return (NodeC (partialTag cn (nargs - length es)) $ args es)
        where
        cn = convertName n
        cons = runIdentity $ getConstructor n dataTable
        nargs = length (conSlots cons)

    con _ = fail "not constructor"


    scInfo tvr | Just n <- Map.lookup (tvrIdent tvr) (scMap cenv) = return n
    scInfo tvr = fail $ "not a supercombinator:" <+> show tvr
    newNodeVar =  fmap (\x -> Var x TyNode) newVar
    newPrimVar ty =  fmap (\x -> Var x ty) newVar
    newNodePtrVar =  fmap (\x -> Var x (TyPtr TyNode)) newVar
    newVar = do
        i <- readIORef (counter cenv)
        writeIORef (counter cenv) $! (i + 2)
        return $! V i

fromRawType (ELit (LitCons tname [] _))
    | RawType <- nameType tname = return (Ty $ toAtom (show tname))
fromRawType _ = fail "not a raw type"

-- | converts an unboxed literal
literal :: Monad m =>  E -> m Val
literal (ELit (LitInt i ty)) | Just ptype <- fromRawType ty = return $ Lit i ptype
literal (EPrim aprim@(APrim p _) xs ty) | Just ptype <- fromRawType ty, primIsConstant p = do
    xs <- mapM literal xs
    return $ ValPrim aprim xs ptype
literal _ = fail "not a literal term"

