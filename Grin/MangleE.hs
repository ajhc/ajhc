-- | This module mangles E code prior to its conversion to grin. among other
-- things, it destroys type information and gets rid of the world so E
-- transformations are no longer safe after this is performed.
--
-- currently it:
--  removes all type coercions.
--  looks for unused types and gets rid of those.
--  atomizes any complex types left in argument position
--
-- the types of unboxed values must remain accurate though.
--

module Grin.MangleE(mangle) where

import Control.Monad.Identity
import Data.Monoid
import Data.Typeable
import Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

import Doc.PPrint
import E.Annotate
import E.E
import E.LetFloat(atomizeApps)
import E.Program
import E.Traverse
import E.TypeCheck(sortKindLike)
import E.Values
import Fixer.Fixer
import Fixer.Supply
import GenUtil
import Info.Info as Info
import Name.Name
import qualified Stats
import Support.FreeVars
import Util.Gen
import Util.Graph


newtype IsUsed = IsUsed Bool
    deriving(Fixable,Eq,Typeable,Show)

-- doesn't consider rules, since they are gone by now
programPruneUnreachable :: Program -> Program
programPruneUnreachable prog = prog { progCombinators = ds' } where
    ds' = reachable (newGraph (progCombinators prog) ( \ (t,_,_) -> tvrIdent t) (\ (t,_,e) -> freeVars e)) (map tvrIdent $ progEntryPoints prog)

--detype = emapFG return (\_ -> return Unknown)
--detypevar tvr = tvr { tvrType = Unknown }

{-# NOINLINE mangle #-}
mangle :: Program -> IO Program
mangle prog = do
    prog <- typeAnalyze prog
    prog <- pruneTypes prog

    stats <- Stats.new
    prog <- return $ atomizeApps True prog -- programMapBodies (atomizeAp True (progDataTable prog) stats (progModule prog)) prog
    prog <- programMapBodies dropCoercions prog
    Stats.print "Mangle" stats

    return (programPruneUnreachable prog)

dropCoercions e = f e where
    f e = emapE f (dropCoerce e)
    dropCoerce e | Just (x,_) <- from_unsafeCoerce e = dropCoerce x
    dropCoerce x = x

typeAnalyze :: Program -> IO Program
typeAnalyze prog = do
    fixer <- newFixer
    funArg <- newSupply fixer
    let idann _ nfo = do
            x <- newValue fixer (IsUsed False)
            return $ Info.insert x nfo
        idread _ nfo | Just v <- Info.lookup nfo = do
            rv <- readValue v
            return (Info.insert (rv :: IsUsed) $ Info.delete (undefined :: Value IsUsed) nfo)
        idread n nfo = return nfo
    prog <- annotateProgram mempty idann (\_ -> return) (\_ -> return) prog
    mapM_ (doComb (Set.fromList $ fsts $ programDs prog) funArg) (progCombinators prog)
    calcFixpoint "MangleE: used types" fixer
    prog <- annotateProgram mempty idread (\_ -> return) (\_ -> return) prog
    putStrLn "Type analyzed methods"
    flip mapM_ (programDs prog) $ \ (t,e) -> do
        let (_,ts) = fromLam e
            ts' = takeWhile good ts
        when (not (null ts')) $ putStrLn $ (pprint t) ++ " \\" ++ concat [ "(" ++ show  (Info.lookup (tvrInfo t) :: Maybe IsUsed) ++ ")" | t <- ts' ]
    return prog

good tvr = sortKindLike (tvrType tvr)

doComb :: Set.Set TVr -> Supply (Int,Int) IsUsed -> (TVr,[TVr],E) -> IO ()
doComb scombs funArg (tvr,as,e) = do
    -- if the argument variable is used, then the argument value is used.
    flip mapM_ (zip naturals as) $ \ (i,ta) -> when (good ta) $ do
        v <- supplyValue funArg (tvrIdent tvr,i)
        tu <- Info.lookup (tvrInfo ta)
        tu `implies` v
    -- process the e
    doE scombs funArg e


doE :: Set.Set TVr -> Supply (Int,Int) IsUsed -> E -> IO ()
doE scombs funArg e = g (value (IsUsed True)) e where
    g eUsed e = f e where
        f (ELit LitCons { litArgs = xs }) = mapM_ f xs
        f ELit {} = return ()
        f (EPrim _ xs _) = mapM_ f xs
        f EError {} = return ()
        f (EVar tvr) = when (good tvr) $ do
            tu <- Info.lookup (tvrInfo tvr)
            eUsed `implies` tu
        f (EPi TVr { tvrType = a } b) = f a >> f b
        f ec@ECase { eCaseScrutinee = e } = do
            f e
            mapM_ f (caseBodies ec)
        f ESort {} = return ()
        f Unknown = return ()
        f (ELetRec ds e) = do
            f e
            sequence_ [ Info.lookup (tvrInfo t) >>= flip g e | (t,e) <- ds, good t ]
            sequence_ [ f e | (t,e) <- ds, not $ good t ]
        f e | (EVar tvr,as) <- fromAp e, tvr `Set.member` scombs =
            flip mapM_ (zip naturals as) $ \ (i,ta) -> do
                v <- supplyValue funArg (tvrIdent tvr,i)
                g v ta
        f (ELam t e) = f e
        f e | (a,as@(_:_)) <- fromAp e = do
            f a
            mapM_ f as


pruneTypes :: Program -> IO Program
pruneTypes prog = do
    (nc,env) <- mconcatMapM pruneC (progCombinators prog)
    let nnc = [ (t,as,pruneE env e) | (t,as,e) <- nc]
    return prog { progCombinators = nnc }

-- It is deliberatly okay to turn a non-CAF into a CAF by getting rid of its type arguments. this is not true for things like getting rid of the World#.

pruneC (tvr,as,e) | not (null missing) = return ([(tvr { tvrType = Unknown }, as', e),ncomb],Map.singleton tvr (ntvr,maximum missing + 1,missing)) where
    missing = [ i | (a,i) <- zip (takeWhile good as) naturals, Info.lookup (tvrInfo a) == Just (IsUsed False)]
    as' = [ a | (a,i) <-  zip as naturals, i `notElem` missing ]
    ntvr = tvr { tvrType = Unknown, tvrIdent = toId nname }
    ncomb = (ntvr,as,foldl EAp (EVar tvr) (map EVar as))
    nname = case fromId (tvrIdent tvr) of
        Just y -> toName Val ("TP@", 'f':show y)
        Nothing -> toName Val ("TP@",'f':show (tvrIdent tvr))
pruneC comb = return ([comb],mempty)

pruneE env e = f e where
    f e | (EVar tvr,as) <- fromAp e = case Map.lookup tvr env of
        Just (ntvr,narg,_) | length as < narg -> foldl EAp (EVar ntvr) (map f as)
        Just (_,_,missing) ->  foldl EAp (EVar tvr) [ f a | (a,i) <-  zip as naturals, i `notElem` missing ]
        _ -> foldl EAp (EVar tvr) (map f as)
    f e = runIdentity $ emapE' (return . f) e


implies :: Value IsUsed -> Value IsUsed -> IO ()
implies x y = addRule $ y `isSuperSetOf` x


