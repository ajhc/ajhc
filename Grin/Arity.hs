module Grin.Arity(grinRaiseArity) where

import IO(stdout)
import qualified Data.Map as Map

import Fixer.Fixer
import Fixer.Supply
import Grin.Grin
import Support.ShowTable
import Support.FreeVars
import GenUtil


grinRaiseArity :: Grin -> IO Grin
grinRaiseArity grin = do
    fixer <- newFixer
    argSupply <- newSupply fixer

    mapM_ (go argSupply) (grinFunctions grin)

    findFixpoint (Just ("grin arity raising",stdout)) fixer

    rv <- supplyReadValues argSupply
    printTable "Grin.Arity: arguments" rv


    return grin


go argSupply (fn,~(Tup as) :-> e) = do
    vs <- mapM (\ (Var v _,i) -> supplyValue argSupply (fn,i)) (zip as naturals)
    let env = Map.fromList (zip [ v | ~(Var v _) <- as ] vs)
        f Fetch {} = return ()
        f (App n as _) = mapM_ (g n) (zip as naturals)
        f (Store (NodeC nn as)) | Just (_,n) <- tagUnfunction nn = mapM_ (g n) (zip as naturals)
        f (e1 :>>= p :-> e2) = f e1 >> f e2
        f (Case x as) = mapM_ bf (freeVars x) >> sequence_ [ f e  | _ :-> e <- as]
        f e = mapM_ bf (freeVars e)
        g fn (Var v _,i) | Just value <- Map.lookup v env = do
            vv <- supplyValue argSupply (fn,i)
            addRule $ vv `implies` value
        g _ _ = return ()
        bf v | Just val <- Map.lookup v env = addRule $ value True `implies` val
        bf _ = return ()
    f e

implies :: Value Bool -> Value Bool -> Rule
implies x y = y `isSuperSetOf` x

