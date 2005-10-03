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
import CanType
import Control.Monad.Identity
import C.Prims
import DataConstructors
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.FreeVars
import E.LambdaLift
import E.Pretty(render)
import E.TypeCheck
import E.Values
import FreeVars
import GenUtil
import Grin.Grin
import Grin.Show
import Grin.Val
import Name
import Options
import PrimitiveOperators
import qualified FlagDump as FD
import Stats
import Util.Graph as G
import VConsts



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
    counter :: IORef Int

}

dumpTyEnv (TyEnv tt) = mapM_ putStrLn $ sort [ fromAtom n <+> hsep (map show as) <+> "::" <+> show t |  (n,(as,t)) <- Map.toList tt]

--compile ::  DataTable -> Map Int Name -> SC -> IO ()
--compile dataTable nmap sc@SC { scMain = mt, scCombinators = cm } = do
--    where

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

--toEntry (n,as,e)
--    --  | Just nm <- Map.lookup (tvrNum n) nmap = ((toAtom ('f':show nm)),map (const $ TyPtr TyNode) as,TyNode)
--    | Just nm <- intToAtom (tvrNum n)  = ((toAtom ('f':show (fromAtom nm :: Name))),map (const $ TyPtr TyNode) as,TyNode)
--    | otherwise = ((toAtom ('f':show (tvrNum n))),map (const $ TyPtr TyNode) as,TyNode)

toEntry (n,as,e)
    | Just nm <- intToAtom (tvrNum n)  = f (toAtom ('f':show (fromAtom nm :: Name)))
    | otherwise = f (toAtom ('f':show (tvrNum n))) where
        f x = (x,map (toType (TyPtr TyNode) . tvrType ) as,toType TyNode (getType (e::E) :: E))

toType :: Ty -> E -> Ty
toType node = toty where
    toty (ELit (LitCons n es ty)) |  ty == eHash, TypeConstructor <- nameType n, Just _ <- fromUnboxedNameTuple n = (TyTup (map (toType (TyPtr TyNode) ) es))
    toty (ELit (LitCons n [] es)) |  es == eHash, RawType <- nameType n = (Ty $ toAtom (show n))
    toty _ = node

compile ::  DataTable -> Map Int Name -> SC -> IO Grin
compile dataTable nmap sc@SC { scMain = mt, scCombinators = cm } = do
    tyEnv <- newIORef initTyEnv
    funcBaps <- newIORef []
    counter <- newIORef 100000  -- TODO real number
    wdump FD.Tags $ do
        dumpTyEnv initTyEnv
    let (cc,reqcc) = constantCaf dataTable sc
    wdump FD.Progress $ do
        putErrLn $ "Found" <+> tshow (length cc) <+> "CAFs to convert to constants," <+> tshow (length reqcc) <+> "of which are recursive."
        putDocMLn putStr $ vcat [ pprint v  | v <- reqcc ]
        putDocMLn putStr $ vcat [ pprint v <+> pprint n <+> pprint e | (v,n,e) <- cc ]
    let doCompile = compile' dataTable CEnv {
            funcBaps = funcBaps,
            tyEnv = tyEnv,
            scMap = scMap,
            counter = counter,
            constMap = mempty,
            ccafMap = Map.fromList [ (tvrNum v,e) |(v,_,e) <- cc]
            }
    ds <- mapM doCompile [ c | c@(v,_,_) <- cm, v `notElem` [x | (x,_,_) <- cc]]
    (_,(Tup [] :-> theMain)) <- doCompile ((mt,[],EAp (EVar mt) vWorld__))
    let tf a = a:tagToFunction a
    ds <- return $ flattenScc $ stronglyConnComp [ (a,x, concatMap tf (freeVars z)) | a@(x,(_ :-> z)) <- ds]
    te <- readIORef tyEnv
    fbaps <- readIORef funcBaps
    --sequence_ [ typecheck te c >>= when False . print . (,) a  | (a,_,c) <-  ds ]
    let (main,as,_) = runIdentity $ Map.lookup (tvrNum mt) scMap
        main' =  if not $ null as then  (Return $ NodeC (partialTag main (length as)) []) else App main []
        tags = Set.toList $ ep $ Set.unions (freeVars (main',initCafs):[ freeVars e | (_,(_ :-> e)) <- ds ])
        ep s = Set.fromList $ concatMap partialLadder $ Set.toList s
        --ev = (funcEval,(Tup [p1] :-> createEval te tags))
        --ap = (funcApply,(createApply te tags))
        cafs = [ ((V $ - atomIndex tag),NodeC tag []) | (x,(Tup [] :-> _)) <- ds, let tag = partialTag x 0 ] ++ [ (y,z') |(x,y,z) <- cc, y `elem` reqcc, let Const z' = z ]
        initCafs = sequenceG_ [ Update (Var v (TyPtr TyNode)) node | (v,node) <- cafs ]
        ic = (funcInitCafs,(Tup [] :-> initCafs) )
        --ds' = ic:ev:ap:ds
        ds' = ic:(ds ++ fbaps)
    --wdump FD.Grin $ do
        --mapM_ putStrLn [ show (x, freeVarsL z :: [Tag]) | (x,_,z) <- ds ]
        --mapM_ (putErrLn . render) $ map prettyFun ds'
    let grin = Grin {
            grinTypeEnv = te,
            --grinFunctions = (funcMain ,[], App funcInitCafs [] :>>= (Unit,Store main') :>>= (p1,gEval p1)): ds',
            --grinFunctions = (funcMain ,(Tup [] :-> App funcInitCafs [] :>>= unit :-> main' :>>= n3 :-> App funcApply [n3,pworld__] )) : ds',
            --grinFunctions = (funcMain ,(Tup [] :-> App funcInitCafs [] :>>= unit :->  main' :>>= n3 :-> App funcApply [n3,pworld__] :>>= n0 :-> Return unit )) : ds',
            grinFunctions = (funcMain ,(Tup [] :-> App funcInitCafs [] :>>= unit :->  theMain :>>= n0 :-> Return unit )) : ds',
            grinCafs = cafs -- [ (n,NodeC t []) | (n,t) <- cafs]
            }
    typecheckGrin grin
    return grin
    where
    scMap = fromList [ (tvrNum t,toEntry x) |  x@(t,_,_) <- scCombinators sc]
    initTyEnv = mappend primTyEnv $ TyEnv $ fromList $ [ (a,(b,c)) | (_,(a,b,c)) <-  Map.toList scMap] ++ [con x| x <- Map.elems $ constructorMap dataTable]
    con c | (ELit (LitCons _ es t),_) <- fromLam $ conExpr c = let
            n | sortStarLike (conType c) = toAtom ('T':show (conName c))
              | otherwise = toAtom ('C':show (conName c))
            as = [ TyPtr TyNode |  EVar tvr <- es]
        in  (n,(as,TyNode))
    con c | (EPi (TVr { tvrType = a }) b,_) <- fromLam $ conExpr c = (tagArrow,([TyPtr TyNode, TyPtr TyNode],TyNode))

        {-
    toTyEnv (n,Tup ps :-> e) = (n,(map (runIdentity . tc initTyEnv) ps,TyNode))
    toEntry (n,as,e)
        | Just nm <- Map.lookup (tvrNum n) nmap = ((toAtom ('f':show nm)),map (toTy.tvrType) as,toTy (typ e))
        | otherwise = ((toAtom ('f':show (tvrNum n))),map (toTy.tvrType) as,toTy (typ e))
    scMap = fromList [ (tvrNum t,toEntry x) |  x@(t,_,_) <- scCombinators sc]
    initTyEnv = TyEnv $ fromList $ [ (a,(b,c)) | (_,(a,b,c)) <-  Map.toList scMap] ++ [con x| x <- Map.elems $ constructorMap dataTable]
    con c = (n,(as,toTy t)) where
        n | sortStarLike (conType c) = toAtom ('T':show (conName c))
          | otherwise = toAtom ('C':show (conName c))
        as = [ toTy $ tvrType tvr |  EVar tvr <- es]
        (ELit (LitCons _ es t),_) = fromLam $ conExpr c
        -}

convertName n = toAtom (t':s) where
    (t,s) = fromName n
    t' | t == TypeConstructor = 'T'
       | t == DataConstructor = 'C'
       | t == Val = 'f'
       | otherwise = error $ "convertName: " ++ show (t,s)

primTyEnv = TyEnv . Map.fromList $ [
    (tagArrow,([TyPtr TyNode, TyPtr TyNode],TyNode)),
    (toAtom "TAbsurd#", ([],TyNode)),
    (funcInitCafs, ([],tyUnit)),
    (funcEval, ([TyPtr TyNode],TyNode)),
    (funcApply, ([TyNode, TyPtr TyNode],TyNode)),
    (tagHole, ([],TyNode))
    ] ++ [ (toAtom ('C':x), ([Ty $ toAtom y],TyNode)) | (x,y) <- allCTypes, y /= "void" ]


-- constant CAF analysis
-- In grin, partial applications are constant data, rather than functions. Since
-- many cafs consist of constant applications, we preprocess them into values
-- beforehand. This also catches recursive constant toplevel bindings.

--type ConstantCafMap = Map.Map Int -> Val
--constantCaf :: DataTable -> SC -> [(TVr,Val)]
constantCaf dataTable (SC _ ds) = ans where
    (lbs',cafs) = G.findLoopBreakers (const 0) $ G.newGraph [ (v,e) | (v,[],e) <- ds, canidate e] (tvrNum . fst) (freeVars . snd)
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
    conv (ELit (LitInt i (ELit (LitCons n [] (ESort EStar))))) | Just pt <- Prelude.lookup (show n) allCTypes = ( Const (NodeC (toAtom $ 'C':show n) [(Lit i (Ty (toAtom pt)))]))
    conv e | Just (a,_) <- from_unsafeCoerce e = conv a
--    conv e | Just (a,_) <- from_integralCast e = conv a
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
--

dropCoerce e | Just (x,_) <- from_unsafeCoerce e = x
--dropCoerce e | Just (x,_) <- from_integralCast e = x
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
    ce (ELetRec ds e) = ce e >>= \e -> doLet ds e
    ce (EError s e) = return (Error s (toType TyNode e))
    ce (EVar tvr@(TVr { tvrType = (ELit (LitCons n [] _))})) | RawType <- nameType n = do
        return (Return (toVal tvr))
    ce e |  (v,as) <- fromAp e, EVar v <- dropCoerce v = do
        as <- return $ args as
        case Map.lookup (tvrNum v) (scMap cenv) of
            Just (_,[],_) -> do
                case constant (EVar v) of
                    Just (Const x) -> app (Return x) as
                    Just x@Var {} -> app (gEval x) as
                    Nothing -> do
                        var@Var {} <- caforconst (EVar v)
                        app (gEval var) as   -- CAFs are looked up in global env
            Just (v,as',es)
                | length as >= length as' -> do
                    let (x,y) = splitAt (length as') as
                    app (App v x) y
                | otherwise -> do
                    let pt = partialTag v (length as' - length as)
                    return $ Return (NodeC pt as)
            Nothing -> app (gEval $ toVal v) as
    ce e | (v,as@(_:_)) <- fromAp e = do
        as <- return $ args as
        e <- ce v
        app e as
    ce (EPi (TVr { tvrIdent = 0, tvrType = a}) b) = do
        a' <- cc a
        b' <- cc b
        p1 <- newNodePtrVar
        p2 <- newNodePtrVar
        return (a' :>>= p1 :-> b' :>>= p2 :-> Return (NodeC tagArrow [p1,p2]))
    ce e | Just (Const z) <- constant e = return (Return z)
    ce e | Just z <- constant e = return (gEval z)
    ce e | Just z <- con e = return (Return z)
    ce e | Just (a,_) <- from_unsafeCoerce e = ce a
    ce ep@(EPrim (APrim (PrimPrim s) _) es _) = do
        fail $ "Unrecognized PrimPrim: " ++ show ep
        return $ App (toAtom $ 'b':s ) (args es)
    ce (EPrim ap@(APrim (Func True fn as "void") _) (_:es) _) = do
        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = (TyTup (map (Ty . toAtom) as),tyUnit), primAPrim = ap }
        return $  Prim p (args es) :>>= unit :-> Return world__
    ce (EPrim ap@(APrim (Func True fn as r) _) (_:es) rt) = do
        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = (TyTup (map (Ty . toAtom) as),Ty (toAtom r)), primAPrim = ap }
            ptv = Var v2 pt
            pt = Ty (toAtom r)
        return $ Prim p (args es) :>>= ptv :-> Return (Tup [pworld__,ptv])
    ce (EPrim ap@(APrim (Func False _ as r) _) es (ELit (LitCons tname [] _))) | RawType <- nameType tname = do
        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = (TyTup (map (Ty . toAtom) as),Ty (toAtom r)), primAPrim = ap }
        return $ Prim p (args es)
    ce (EPrim ap@(APrim (Peek pt') _) [_,addr] rt) = do
        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = (TyTup [Ty (toAtom "HsPtr")],pt), primAPrim = ap }
            ptv = Var v2 pt
            pt = Ty (toAtom pt')
        return $  Prim p (args [addr]) :>>= ptv :-> Return (Tup [pworld__,ptv])
    ce (EPrim ap@(APrim (Poke pt') _) [_,addr,val] _) = do
        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = (TyTup [Ty (toAtom "HsPtr"),pt],tyUnit), primAPrim = ap }
            pt = Ty (toAtom pt')
        return $  Prim p (args [addr,val]) :>>= unit :-> Return world__
    ce (EPrim aprim@(APrim (AddrOf s) _) [] (ELit (LitCons tname [] _))) | RawType <- nameType tname = do
        let p = Primitive { primName = toAtom ('&':s), primRets = Nothing, primType = (tyUnit,ptype), primAPrim = aprim }
            ptype = Ty $ toAtom (show tname)
        return $ Prim p []
    ce (EPrim aprim@(APrim (CConst s t) _) [] (ELit (LitCons n [] _))) | RawType <- nameType n = do
        let p = Primitive { primName = toAtom s, primRets = Nothing, primType = (tyUnit,ptype), primAPrim = aprim }
            ptype = Ty $ toAtom t
        return $ Prim p []
--    ce (EPrim ap@(APrim (Peek pt') _) [_,addr] rt) = do
--        (v,b,w) <- cpa addr
--        (c,_) <- fromIORT rt
--        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = (TyTup [Ty (toAtom "HsPtr")],pt), primAPrim = ap }
--            ptv = Var v2 pt
--            pt = Ty (toAtom pt')
--        return $ w :>>= b :-> Prim p [v] :>>= ptv :->  Store (NodeC (toAtom $ 'C':show c) [ptv]) :>>= p3 :-> retIO p3
--    ce (EPrim ap@(APrim (Poke pt') _) [_,addr,val] _) = do
--        (v,b,w) <- cpa addr
--        (v',b',w') <- cpa val
--        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = (TyTup [Ty (toAtom "HsPtr"),pt],tyUnit), primAPrim = ap }
--            ptv = Var v2 pt
--            pt = Ty (toAtom pt')
--        return $ w :>>= b :-> w' :>>= b' :-> Prim p [v,v'] :>>= unit :-> Return world__
--    ce (EPrim ap@(APrim (Func True fn as "void") _) (_:es) _) = do
--        es' <- mapM cpa es
--        let fr = foldl (.) id [ (\e -> w :>>= b :-> e) | (_,b,w) <- es' ]
--            p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = (TyTup (map (Ty . toAtom) as),tyUnit), primAPrim = ap }
--        return $ fr ( Prim p [ x | (x,_,_) <- es' ] :>>= unit :-> Return world__)
--    ce (EPrim ap@(APrim (Func True fn as r) _) (_:es) rt) = do
--        es' <- mapM cpa es
--        (c,rr) <- fromIORT rt
--        let fr = foldl (.) id [ (\e -> w :>>= b :-> e) | (_,b,w) <- es' ]
--            p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = (TyTup (map (Ty . toAtom) as),Ty (toAtom r)), primAPrim = ap }
--            ptv = Var v2 pt
--            pt = Ty (toAtom r)
--        return $ fr ( Prim p [ x | (x,_,_) <- es' ] :>>= ptv :-> Store (NodeC (toAtom $ 'C':show c) [ptv]) :>>= p3 :-> retIO p3)
    --ce (EPrim aprim@(APrim (AddrOf s) _) [] t) | Just (c,ptype') <- lookupCType dataTable t = do
    --    let cname = 'C':show c
    --        ptype = Ty $ toAtom ptype'
    --    let p = Primitive { primName = toAtom ('&':s), primRets = Nothing, primType = (tyUnit,ptype), primAPrim = aprim }
    --    return $ Prim p [] :>>= Var v1 ptype :-> Return (NodeC (toAtom cname) [Var v1 ptype])
    --ce (EPrim aprim@(APrim (CConst s _) _) [] t) |  Just (c,ptype') <- lookupCType dataTable t = do
    --    let cname = 'C':show c
    --        ptype = Ty $ toAtom ptype'
    --    let p = Primitive { primName = toAtom s, primRets = Nothing, primType = (tyUnit,ptype), primAPrim = aprim }
    --    return $ Prim p [] :>>= Var v1 ptype :-> Return (NodeC (toAtom cname) [Var v1 ptype])
    ce ee@(EPrim aprim@(APrim (CCast from to) _) [e] t)  = do
        let ptypeto' = Ty $ toAtom to
            ptypefrom' = Ty $ toAtom from
        let p = Primitive { primName = toAtom ("(" ++ to ++ ")"), primRets = Nothing, primType = (TyTup [ptypefrom'],ptypeto'), primAPrim = aprim }
        return $  Prim p (args [e])
    ce (EPrim ap@(APrim (Operator n as r) _) es (ELit (LitCons tname [] _))) | RawType <- nameType tname = do
        let p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = (TyTup (map (Ty . toAtom) as),Ty (toAtom r)), primAPrim = ap }
        return $ Prim p (args es)
        {-
    ce (EPrim ap@(APrim (Operator n as r) _) es rt) = do
        es' <- mapM cpa es
        Just (c,rr) <- return $ lookupCType dataTable rt
        True <- return $ rr == r
        let fr = foldl (.) id [ (\e -> w :>>= b :-> e) | (_,b,w) <- es' ]
            p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = (TyTup (map (Ty . toAtom) as),Ty (toAtom r)), primAPrim = ap }
            ptv = Var v2 pt
            pt = Ty (toAtom r)
        return $ fr ( Prim p [ x | (x,_,_) <- es' ] :>>= ptv :-> Return (NodeC (toAtom $ 'C':show c) [ptv]) )
        {-
    ce (EPrim ap@(APrim (Func False fn as r) _) es rt) = do
        es' <- mapM cpa es
        Just (c,rr) <- return $ lookupCType dataTable rt
        let fr = foldl (.) id [ (\e -> w :>>= b :-> e) | (_,b,w) <- es' ]
            p = Primitive { primName = Atom.fromString (pprint ap), primRets = Nothing, primType = (TyTup (map (Ty . toAtom) as),Ty (toAtom r)), primAPrim = ap }
            ptv = Var v2 pt
            pt = Ty (toAtom r)
        return $ fr ( Prim p [ x | (x,_,_) <- es' ] :>>= ptv :-> Return (NodeC (toAtom $ 'C':show c) [ptv]) )
        -}
    ce ee@(EPrim aprim@(APrim (CCast from to) _) [e] t)  = do
        fd <- ce e
        Just (cfrom,ptypefrom) <- return $ lookupCType dataTable (typ e)
        Just (cto,ptypeto) <- return $ lookupCType dataTable t
        unless (ptypefrom == from && ptypeto == to) $ fail ("CCast no match: " ++ show ee)
        let namecto = toAtom $ 'C':show cto
            ptypeto' = Ty $ toAtom ptypeto
            namecfrom = toAtom $ 'C':show cfrom
            ptypefrom' = Ty $ toAtom ptypefrom
            vfrom = Var v1 ptypefrom'
            vto = Var v2 ptypeto'
        let p = Primitive { primName = toAtom ("(" ++ to ++ ")"), primRets = Nothing, primType = (TyTup [ptypefrom'],ptypeto'), primAPrim = aprim }
        return $ fd :>>= NodeC namecfrom [vfrom] :-> Prim p [vfrom] :>>= vto :-> Return (NodeC namecto [vto])

        x1 <- ce e1
        x2 <- ce e2
        (cons1,ctp1) <- lookupCType dataTable (typ e1)
        (cons2,ctp2) <- lookupCType dataTable (typ e2)
        (consr,ctpr) <- lookupCType dataTable t
        True <- return $ t1 == ctp1
        True <- return $ t2 == ctp2
        True <- return $ rt == ctpr

        let cname = 'C':show c
        let p1 = Var v1 ptype
            p2 = Var v2 ptype
            p3 = Var v3 ptype
            Just ptype' = Prelude.lookup (show c) allCTypes
            node x = NodeC (toAtom cname) [x]
            ptype = Ty $ toAtom ptype'
        let p = Primitive { primName = toAtom s, primRets = Nothing, primType = (TyTup [ptype,ptype],ptype), primAPrim = aprim }
        return $ x1 :>>= node p1 :-> x2 :>>= node p2 :-> Prim p [p1,p2] :>>= p3 :-> Return (node p3)
        -}
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

    ce (ECase e b as d) | any isJust  ls = ans where
        ans = do
            v <- newPrimVar ty
            nv <- newNodeVar
            e <- ce e
            as' <- mapM (cp' v cons) as
            def <- createDef d (newPrimVar ty)
            return $
                e :>>= nv :->
                Store nv :>>= toVal b :->
                Return nv :>>= NodeC cons [v] :->
                Case v (as' ++ def)
        ls = map isBasic [ a | Alt a _ <- as]
        Just (cons,ty) = msum ls
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
    cpa :: E -> IO (Val,Val,Exp)
    cpa e = do
        v <- newVar
        (c,t) <- case lookupCType dataTable (getType e) of
            Right x -> return x
            Left m -> fail (m <+> show e)
        let var = Var v (Ty $ toAtom t)
        w <- ce e
        return (var,NodeC (toAtom $ 'C':show c) [var],w)

    isBasic ( (LitInt _ t)) | Just (c,pt) <- lookupCType dataTable t = return (toAtom $ 'C':show c, Ty $ toAtom pt)
    --isBasic ( (LitInt _ (ELit (LitCons c [] _)))) | Just pt <- Prelude.lookup (show c) allCTypes = return (toAtom $ 'C':show c, Ty $ toAtom pt)
    --isBasic (PatLit (LitChar {})) = return tCharzh
    isBasic e = fail $ "Not Basic: " ++ show e
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
        --let (e',as') = fromLam e
        x <- ce e
        --es <- mapM (\_ -> newNodePtrVar) (drop (length as') es)
        --x <- app x es
        nn <- getName lc
        return (NodeC nn (map toVal es) :-> x)
    cp x = error $ "cp: " ++ show (funcName,x)
    cp'' (Alt (LitInt i (ELit (LitCons nn [] _))) e) = do
        x <- ce e
        return (Lit i (Ty $ toAtom (show nn)) :-> x)

    cp' nv cons p = f p where
        f (Alt l@(LitInt i _) e) = do
            x <- ce e
            --z <- const $ ELit l
            (_,tp) <- isBasic l
            z <- return $ Lit i tp
            return (z :-> x)
--        f (PatLit l@(LitChar i),e) = do
--            x <- ce e
--            --z <- const $ ELit l
--            z <- return $ Lit (ord i) tCharzh
--            return (z :-> x)
--        f (PatWildCard,ELam (TVr Nothing _)  e) = do
--            x <- ce e
--            nv <- newPrimVar (Ty cons)
--            return (nv :-> x)
--        f (PatWildCard,ELam tvr e) = do
--            x <- ce e
--            nv' <- newPrimVar (Ty cons)
--            return (nv' :-> Store (NodeC cons [nv]) :>>= toVal tvr :-> x)
--        f (PatWildCard,e) = do
--            x <- ce e
--            w <- newNodePtrVar
--            m <- newNodeVar
--            nv' <- newPrimVar (Ty cons)
--            return (nv' :-> Store (NodeC cons [nv]) :>>= w :-> x :>>= m :-> gApply m w)

    getName x = getName' dataTable x
{-
    getName v@(LitCons n es _)
        | conAlias cons = error $ "Alias still exists: " ++ show v
        | length es == nargs  = do
            return cn
        | nameType n == TypeConstructor && length es < nargs = do
            return ((partialTag cn (nargs - length es)))
        where
        cn = convertName n
        cons = runIdentity $ getConstructor n dataTable
        nargs = length (conSlots cons)
-}
{-
    cp (PatLit (LitCons n es _),e) = do
        x <- ce e
        es <- mapM (\_ -> newNodePtrVar) es
        x <- app x es
        return (NodeC (convertName n) es :-> x)
-}

    app e [] = return e
    app e (a:as) = do
        v <- newNodeVar
        app (e :>>= v :-> gApply v a) as
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
        let t = toAtom $ "Bap_" ++ show (length as) ++ "_" ++ funcName ++ "_" ++ show vn
            tl = toAtom $ "bap_" ++ show (length as) ++ "_" ++  funcName ++ "_" ++ show vn
            args = [Var v (TyPtr TyNode) | v <- [v1..] | _ <- (undefined:as)]
            s = Store (NodeC t (e:as))
        d <- app (gEval p1) (tail args)
        addNewFunction (tl,Tup (args) :-> d)
        --modifyIORef (funcBaps cenv) ((tl,Tup (args) :-> d):)
        --let addt (TyEnv mp) =  TyEnv $ Map.insert tl (replicate (length args) (TyPtr TyNode),TyNode) mp
        --modifyIORef (tyEnv cenv) addt
        return s
    addNewFunction tl@(n,Tup args :-> body) = do
        modifyIORef (funcBaps cenv) (tl:)
        tenv <- readIORef (tyEnv cenv)
        args' <- mapM (typecheck tenv) args
        rb <- typecheck tenv body
        let addt (TyEnv mp) =  TyEnv $ Map.insert n (args',rb) mp
        modifyIORef (tyEnv cenv) addt

    {-
    app' e (a:as) = do
        v <- newNodePtrVar
        let s = Store (NodeC tagApply [e,a])
        r <- app' v as
        return (s :>>= v :-> r)
    -}

    --cc e | Just c <- const e = do
    --    return (Return (Const c))
    --cc (EPi (TVr 0 a) b) = do
    --    a' <- cc a
    --    b' <- cc b
    --    p1 <- newNodePtrVar
    --    p2 <- newNodePtrVar
    --    return (a' :>>= p1 :-> b' :>>= p2 :-> Store (NodeC tagArrow [p1,p2]))
    cc e | Just z <- constant e = return (Return z)
    cc e | Just z <- con e = do
        return (Store z)
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
                    --fail "thinking still..."
                | otherwise -> do
                    let pt = partialTag v (length as' - length as)
                    return $ Store (NodeC pt as)
            Nothing
                | [] <- as -> return $ Return (toVal v)
                | otherwise  -> app' (toVal v) as
    cc e | Just (x,_) <- from_unsafeCoerce e = cc x
--    cc e | Just (x,_) <- from_integralCast e = cc x
--    cc (EPrim aprim@(APrim (PrimPrim s) _) es pt) = do
--        V vn <- newVar
--        te <- readIORef (tyEnv cenv)
--        let fn = toAtom ('b':s ++ "_" ++ show vn)
--            fn' = toAtom ('B':s ++ "_" ++ show vn)
--        case findArgsType te fn of
--            Just _ -> return $ Store $ NodeC fn' (args es)
--            Nothing -> do
--                let es' = args es
--                ts <- mapM (typecheck te) es'
--                let nvs = [ Var v t | t <- ts | v <- [v2,V 4..] ]
--                x <- ce (EPrim aprim [ EVar (TVr v t) | t <- map typ es | v <- [2,4..]] pt)
--                addNewFunction (fn,Tup nvs :-> x)
--                return $ Store $ NodeC fn' es'
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
    --cc (EPrim (APrim (PrimPrim s) _) es _) = do
    --    return $ Store $ NodeC (toAtom $ "B" ++ s ) (args es)
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

                    --return (e :>>= v :->
                    --        Fetch v :>>= v' :->
                    --        Update (toVal tvr) v')
            xs <- mapM u bs
            v <- f ds x
            let r = (foldr (\a b -> a :>>= unit :-> b) v xs)
            return $ foldr g r bs

            --fail "can't handle recursion just yet."
    -- This avoids a blind update on recursive thunks
    doUpdate vr (Store n) = Update vr n
    doUpdate vr (x :>>= v :-> e) = x :>>= v :-> doUpdate vr e
    doUpdate vr x = error $ "doUpdate: " ++ show x
    args es = map f es where
        --f x | Just z <- caforconst x =  z
        f x | Just z <- constant x =  z
        f (EVar tvr) = toVal tvr
        f e | Just (x,_) <- from_unsafeCoerce e = f x
--        f e | Just (x,_) <- from_integralCast e = f x
        f x = error $ "invalid argument: " ++ show x

    -- Takes an E and returns something constant of type (TyPtr TyNode)
    constant :: Monad m =>  E -> m Val
    constant (EVar tvr) | Just c <- Map.lookup (tvrNum tvr) (ccafMap cenv) = return c
                        | Just (v,as,_) <- Map.lookup (tvrNum tvr) (scMap cenv)
                         , t <- partialTag v (length as)  = case tagIsWHNF t of
                            True -> return $ Const $ NodeC t []
                            False -> return $ Var (V $ - atomIndex t) (TyPtr TyNode)
                            --case constant e of
                            --    Just x -> return x
                            --    Nothing -> return $ Var (V $ - atomIndex t) (TyPtr TyNode)
    constant (ELit (LitInt i (ELit (LitCons n [] (ESort EHash))))) | RawType <- nameType n = return $ Lit i (Ty $ toAtom (show n))
    constant (ELit (LitInt i (ELit (LitCons n [] (ESort EStar))))) | Just pt <- Prelude.lookup (show n) allCTypes = (return $ Const (NodeC (toAtom $ 'C':show n) [(Lit i (Ty (toAtom pt)))]))
--    constant (ELit lc@(LitCons n es _)) | Just es <- mapM constant es, Just _ <- fromUnboxedNameTuple n, DataConstructor <- nameType n = (return $ Const (Tup es))
    constant (ELit lc@(LitCons n es _)) | Just es <- mapM constant es, Just nn <- getName lc = (return $ Const (NodeC nn es))
    constant (EPi (TVr { tvrIdent = 0, tvrType = a}) b) | Just a <- constant a, Just b <- constant b = return $ Const $ NodeC tagArrow [a,b]
    constant e | Just (a,_) <- from_unsafeCoerce e = constant a
--    constant e | Just (a,_) <- from_integralCast e = constant a
    constant _ = fail "not a constant term"

    caforconst e = constant e

    {-
    caforconst :: Monad m =>  E -> m Val
    caforconst (EVar tvr)  | Just (v,as,_) <- Map.lookup (tvrNum tvr) (scMap cenv)
                         , t <- partialTag v (length as)  = case tagIsWHNF t of
                            True -> return $ Const $ NodeC t []
                            False -> return $ Var (V $ - atomIndex t) (TyPtr TyNode)
    caforconst e = liftM Const $ const e

    const :: Monad m => E -> m Val  --needed for polymorphic recursion
    const (EVar tvr)  | Just (v,as,_) <- Map.lookup (tvrNum tvr) (scMap cenv)
                         , t <- partialTag v (length as)  = case tagIsWHNF t of
                            True -> return $ NodeC t []
                            False -> fail "const: CAF"
    --const (ELit (LitInt i t)) | t == tChar = (return (NodeC (toAtom "CChar") [(Lit ( fromIntegral i) tCharzh)]))
    --const (ELit (LitInt i t))   = (return (NodeC (toAtom "CInt") [(Lit (fromIntegral i) tIntzh)]))
    const (ELit (LitInt i (ELit (LitCons n [] (ESort 0))))) | Just pt <- Prelude.lookup (show n) allCTypes = (return (NodeC (toAtom $ 'C':show n) [(Lit (fromIntegral i) (Ty (toAtom pt)))]))
    const (ELit lc@(LitCons n es _)) | Just es <- mapM const es, Just nn <- getName lc = (return (NodeC nn (map Const es)))
    const (EPi (TVr 0 a) b) | Just a <- const a, Just b <- const b = return $ NodeC tagArrow [Const a,Const b]
    --const e@(EPi {}) | (ELit (LitCons n as' t),as) <- fromPi e, as == [ v | EVar v <- as'] = const (ELit (LitCons n [] undefined))
    const _ = fail "not a constant term"
      -}
    con :: Monad m => E -> m Val
    --con e | Just z <- const e = return z
    --con e | Just (Const z) <- constant e = return z
    con (EPi (TVr {tvrIdent =  0, tvrType = x}) y) = do
        return $  NodeC tagArrow (args [x,y])
    con v@(ELit (LitCons n es _))
        | conAlias cons = error $ "Alias still exists: " ++ show v
        | Just v <- fromUnboxedNameTuple n, DataConstructor <- nameType n = do
            return (Tup (args es))
        | length es == nargs  = do
            return ((NodeC cn (args es)))
        | nameType n == TypeConstructor && length es < nargs = do
            return (NodeC (partialTag cn (nargs - length es)) $ args es)
        where
        cn = convertName n
        cons = runIdentity $ getConstructor n dataTable
        nargs = length (conSlots cons)

    con _ = fail "not constructor"


    scInfo tvr | Just n <- Map.lookup (tvrNum tvr) (scMap cenv) = return n
    scInfo tvr = fail $ "not a supercombinator:" <+> show tvr
    --newVar' = fmap (\x -> Var x TyNode) newVar
    newNodeVar =  fmap (\x -> Var x TyNode) newVar
    newPrimVar ty =  fmap (\x -> Var x ty) newVar
    newNodePtrVar =  fmap (\x -> Var x (TyPtr TyNode)) newVar
    newVar = do
        i <- readIORef (counter cenv)
        writeIORef (counter cenv) $! (i + 2)
        return $! V i

    --ce (EPi (TVr Nothing x) y) = do
    --    return $ Return $ NodeC (toAtom "T->") (args [x,y])
    --ce (ELit (LitCons n es _)) = do
    --    return (Return (NodeC (convertName n) (args es)))
--        f (EVar tvr)
--            | Just (v,as,_) <- Map.lookup (tvrNum tvr) (scMap cenv) =
--                    let pt = partialTag v (length as) in
--                      (Const $ NodeC pt [])
--            | otherwise  = toVal tvr
    --cc e@EPi {} = do
    --   v <- newNodeVar
    --    x <- ce e
    --    return (x :>>= (v,Store v))

--    ce e | Just (a,_) <- from_integralCast e = ce a
    --ce (EPrim "seq" [a,b] _) = do
    --    a <- ce a
    --    b <- ce b
    --    return $ a :>>= n0 :-> b
--    ce (EPrim aprim@(APrim (PrimPrim s) _) [] t) | "prim_const." `isPrefixOf` s, Just (c,ptype') <- lookupCType dataTable t = do
--        let cname = 'C':show c
--        --let Just ptype' = Prelude.lookup (show c) allCTypes
--            ptype = Ty $ toAtom ptype'
--        let p = Primitive { primName = toAtom s, primRets = Nothing, primType = (tyUnit,ptype), primAPrim = aprim }
--        return $ Prim p [] :>>= Var v1 ptype :-> Return (NodeC (toAtom cname) [Var v1 ptype])
--    ce (EPrim aprim@(APrim (PrimPrim s) _) [e] (ELit (LitCons c [] (ESort 0)))) | "prim_op_aa." `isPrefixOf` s = do
--        x <- ce e
--        let cname = 'C':show c
--        let p1 = Var v1 ptype
--            p2 = Var v2 ptype
--            node x = NodeC (toAtom cname) [x]
--            Just ptype' = Prelude.lookup (show c) allCTypes
--            ptype = Ty $ toAtom ptype'
--        let p = Primitive { primName = toAtom s, primRets = Nothing, primType = (TyTup [ptype],ptype), primAPrim = aprim }
--        return $ x :>>= node p1 :-> Prim p [p1] :>>= p2 :-> Return (node p2)
--    ce (EPrim aprim@(APrim (PrimPrim s) _) [e1,e2] (ELit (LitCons c [] (ESort 0)))) | "prim_op_aaa." `isPrefixOf` s = do
--        x1 <- ce e1
--        x2 <- ce e2
--        let cname = 'C':show c
--        let p1 = Var v1 ptype
--            p2 = Var v2 ptype
--            p3 = Var v3 ptype
--            Just ptype' = Prelude.lookup (show c) allCTypes
--            node x = NodeC (toAtom cname) [x]
--            ptype = Ty $ toAtom ptype'
--        let p = Primitive { primName = toAtom s, primRets = Nothing, primType = (TyTup [ptype,ptype],ptype), primAPrim = aprim }
--        return $ x1 :>>= node p1 :-> x2 :>>= node p2 :-> Prim p [p1,p2] :>>= p3 :-> Return (node p3)
--    ce (EPrim aprim@(APrim (PrimPrim s) _) [e1,e2] (ELit (LitCons c [] (ESort 0)))) | "prim_op_aaa." `isPrefixOf` s = do
--        x1 <- ce e1
--        x2 <- ce e2
--        let cname = 'C':show c
--        let p1 = Var v1 ptype
--            p2 = Var v2 ptype
--            p3 = Var v3 ptype
--            Just ptype' = Prelude.lookup (show c) allCTypes
--            node x = NodeC (toAtom cname) [x]
--            ptype = Ty $ toAtom ptype'
--        let p = Primitive { primName = toAtom s, primRets = Nothing, primType = (TyTup [ptype,ptype],ptype), primAPrim = aprim }
--        return $ x1 :>>= node p1 :-> x2 :>>= node p2 :-> Prim p [p1,p2] :>>= p3 :-> Return (node p3)
--    ce (EPrim aprim@(APrim (PrimPrim s) _) [e1,e2] tBool) | "prim_op_aaB." `isPrefixOf` s = do
--        x1 <- ce e1
--        x2 <- ce e2
--        let cname = 'C':show c
--            p1 = Var v1 ptype
--            p2 = Var v2 ptype
--            p3 = Var v3 intT
--            (ELit (LitCons c [] (ESort 0))) = followAliases dataTable (typ e1)
--            intT =   (Ty (toAtom "int"))
--            Just ptype' = Prelude.lookup (show c) allCTypes
--            node x = NodeC (toAtom cname) [x]
--            ptype = Ty $ toAtom ptype'
--            p = Primitive { primName = toAtom s, primRets = Nothing, primType = (TyTup [ptype,ptype],intT), primAPrim = aprim }
--        return $ x1 :>>= node p1 :-> x2 :>>= node p2 :-> Prim p [p1,p2] :>>= p3 :-> Case p3 [Lit 0 intT :-> Return vFalse, Var v0 intT :-> Return vTrue]
