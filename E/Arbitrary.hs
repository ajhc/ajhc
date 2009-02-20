module E.Arbitrary where

--import Test.QuickCheck
import E.E
import E.TypeCheck()
import qualified Data.Set as Set
import qualified Data.Map as Map
import Random
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty (putDoc, Doc)
import E.Show
import Data.Monoid
import GenUtil
import Support.FreeVars
import Name.VConsts
import Support.CanType
import CharIO
import Name.Id



data EP = EP {
    canDiverge :: Bool,
    usedVars :: Set.Set TVr
    }


choose :: [IO a] -> IO a
choose [] = fail "nothing to choose from"
choose as = do
    x <- randomRIO (0,length as - 1)
    as !! x

value t
    | t == tInteger = choose [return $ ELit $ LitInt 1 tInteger]
    | t == tChar = choose [return $ ELit $ LitInt (fromIntegral (fromEnum 'x')) tChar]
    | t == eStar = choose $ map return [tChar, tInteger]

var t = do
    x <- randomRIO (1,100)
    return $ TVr (anonymous $ 2*x) t mempty

complicate :: Set.Set TVr -> E -> IO E
complicate fvs e = do
    let re = if Set.null fvs then (replicate 1 (return e) ++ ) else id
    e <- choose  $ re  (replicate 2 $ complicate' fvs e >>= complicate fvs )
    return e

complicate' fvs e
    | EPi (TVr _ a1 _) a2 <- te = choose [ do v <- value a1; return (EAp e v), f e  ]
    | otherwise = f e
    where
    te = getType e
    f (EAp a b) = choose [do
        a' <- complicate fvs a
        b' <- complicate fvs b
        return (EAp a b), g e]
    f (ELam v e) = choose [do
            e' <- complicate fvs e
            return (ELam v e'),
            g e]
    f e = g e
    g e = do
        t <- value eStar
        v <- var t
        return (ELam v e)

genE = do
    v <- value tInteger
    complicate mempty v

ge = do
    e <- genE
    CharIO.print e
    CharIO.putStrLn (render $ (ePretty e :: Doc))
    putDoc (pprint (getType e))

testE = do
    CharIO.putStrLn "Testing E"
    ge
    ge


{-

typeSet env = Map.keys env
typeCnt env a
    | Just x <- Map.lookup a env = x
    | otherwise = 0
typeCntInc env a = Map.insert a (typeCnt env a + 1) env


countTerm :: E -> Map.Map E Int -> Int -> Int
countTerm _t _env 0 = 0
countTerm t env 1 = typeCnt env t
countTerm t@(EPi (TVr _ a1 _) a2) env s = countTerm a2 (typeCntInc env a1) (s - 1) + countHeadVarTerm t env s
countTerm t env s = countHeadVarTerm t env s

countHeadVarTerm t env s = sum [ countHeadVarArgTerms bs env s | bs <- validHeadVarTypeSet t env]

countHeadVarArgTerms (b,bs) env s
    | numVarWithTypeInEnv <= 0 = 0    -- multiplication is too strict here
    | otherwise = numVarWithTypeInEnv * numTerms where
        numVarWithTypeInEnv = typeCnt env b
        m = length bs
        numTerms = sum [ product [ countTerm b env s | s <- ss | b <- bs ] | ss <- ndk (s - 1 - m) m]

validHeadVarTypeSet  a env = concat (map (f []) (Map.keys env)) where
    f rs b | b == a = return (a,reverse rs)
    f rs (EPi (TVr _ b1 _) b2) = f (b1:rs) b2
    f _ _ = fail "not valid head var type set"

ndk :: Int -> Int -> [[Int]]
ndk n m | n < 0 = error "ndk: n < 0"
ndk n m | m < 0 = error "ndk: m < 0"
ndk n m | n < m = error "ndk: n < m"
ndk 0 m = []
ndk n m | n == m = [replicate m 1]
ndk n m = snub $ concat [ f ss | ss <- ndk (n - 1) m] where
    f ss = [ [ if i == i' then s + 1 else s | s <- ss | i' <- [0 :: Int ..] ] | _ <- ss | i <- [0 :: Int ..] ]

testE = do
    putStrLn "Testing E"
    let f x i = do
        putStrLn $ "countTerm" <+> show x <+> show i <+> "=" <+> show (countTerm x (Map.singleton eStar 1) i)
    f eStar 2
    f eStar 4
    f eStar 1
    f (ePi (TVr 0 eStar mempty) eStar) 7
    let prop_ndk n m = abs n >= abs m ==> let ss = ndk (abs n) (abs m) in and [ sum s == (abs n) |s <- ss] && unique mempty ss where
        unique _ [] = True
        unique ss (x:xs) | x `Set.member` ss = False
        unique ss (x:xs) = unique (Set.insert x ss) xs
    --quickCheck prop_ndk


    print (ndk 4 2)
    --print (ndk 10 4)

gen a s = genTerm a mempty s

genTerm _a _env s | s < 1 = return Nothing
genTerm a env 1
    | typeCnt env a > 0 = genVarTerm env a
    | otherwise = return Nothing
genTerm (EPi (TVr _ a1) a2) env s = do
    let totalNumTerm = countTerm a env s
        numLamTerm = countTerm a2 (typeCntInc env a1 (s - 1))
    x <- randomRIO (0,totalNumTerm)
    if x <= numLamTerm then
        genLamTerm a1 a2 env s
      else genAppTerm a env s (totalNumTerm  - numLamTerm)
genTerm a env s = genAppTerm a env s (countTerm a env s)

genVarTerm a env | typeCnt env a == 0 = return Nothing

-}

