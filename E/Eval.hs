module E.Eval(eval, strong) where

-- Simple lambda Calculus interpreter
-- does not handle recursive Let or Case statements, but those don't appear in types anyway.

import Control.Monad.Writer
import qualified Data.Map as Map

import Doc.DocLike
import Doc.PPrint
import E.E
import E.FreeVars
import {-# SOURCE #-} E.Show
import E.Subst



eval :: E -> E
eval term = eval' term []  where
    -- final terms
    eval' t@EVar {} [] = t
    eval' (ELam v body) [] = check_eta $ ELam v (eval body)
    eval' (EPi v body) [] = check_eta $ EPi v (eval body)
    eval' e@Unknown [] = e
    eval' e@ESort {} [] = e
    eval' (ELit lc@LitCons { litArgs = es }) [] = ELit lc { litArgs = map eval es }
    eval' e@ELit {} [] = e

    -- argument applications
    eval' (ELit lc@LitCons { litArgs = es, litType = EPi tb tt }) (t:rest) = eval' (ELit lc { litArgs = es ++ [t], litType = subst tb t tt }) rest
    eval' (ELit LitCons { litArgs = es, litAliasFor = Just af }) (t:rest) = eval' af (es ++ t:rest)

    eval' (ELam v body) (t:rest) = eval' (subst v t body) rest
    eval' (EPi v body) (t:rest) = eval' (subst v t body) rest   -- fudge
    eval' (EAp t1 t2) stack = eval' t1 (t2:stack)
    eval' t@EVar {} stack = unwind t stack
    eval' ELetRec { eDefs = ds, eBody = e } stack = eval' (f (decomposeDs ds) e) stack where
        f [] e = e
        f (Left (x,y):ds) e =  subst x y (f ds e)
        f (Right _:_) _ = error $ "cannot eval recursive let"
    eval' e@(ELit LitCons {}) stack = unwind e stack
    eval' e _ = error $ "Cannot eval: " ++ show e

    unwind t [] = t
    unwind t (t1:rest) = unwind (EAp t $ eval t1) rest

    -- currently we do not do eta check. etas should only appear for good reason.
    check_eta x = x




strong :: Monad m => [(TVr,E)] -> E -> m E
strong dsMap' term = eval' dsMap term [] where
    dsMap = Map.fromList dsMap'
    --eval' ds t@EVar {} [] = t
    etvr ds tvr = do
        t' <- (eval' ds (tvrType tvr) [])
        return $ tvr { tvrType = t' }
    eval' :: Monad m => Map.Map TVr E -> E -> [E] -> m E

    eval' ds (ELam v body) [] = do
        let ds' = Map.delete v ds
        v' <- etvr ds' v
        body' <- (eval' ds' body [])
        check_eta $ ELam v' body'
    eval' ds (EPi v body) [] = do
        let ds' = Map.delete v ds
        body' <- (eval' ds' body [])
        v' <- etvr ds' v
        check_eta $ EPi v' body'
    eval' ds e@Unknown [] = return e
    eval' ds e@ESort {} [] = return e
    eval' ds (ELit lc@LitCons { litArgs = es, litType = t }) [] = do
        es' <- mapM (\e -> eval' ds e []) es
        t' <-  (eval' ds t [])
        return $ ELit $ lc { litArgs = es', litType = t' }
    eval' ds e@ELit {} [] = return e
    eval' ds (ELit lc@LitCons { litArgs = es, litType = EPi tb tt }) (t:rest) = eval' ds (ELit lc { litArgs = es ++ [t], litType = subst tb t tt }) rest
    eval' ds (ELit LitCons { litArgs = es, litAliasFor = Just af }) (t:rest) = eval' ds af (es ++ t:rest)
    eval' ds (ELam v body) (t:rest) = eval' ds (subst v t body) rest
    eval' ds (EPi v body) (t:rest) = eval' ds (subst v t body) rest   -- fudge
    eval' ds (EAp t1 t2) stack = eval' ds t1 (t2:stack)
    eval' _ds (EVar TVr { tvrIdent = 0 }) _stack = fail "empty ident in term"
    eval' ds t@(EVar v) stack
        | Just x <- Map.lookup v ds = eval' ds x stack
        | otherwise = do
            tvr <- etvr ds v
            unwind ds (EVar tvr) stack
    eval' ds ELetRec { eDefs = ds', eBody = e } stack = eval' (Map.fromList ds'  `mappend` ds) e  stack
    eval' ds e@(ELit LitCons {}) stack = unwind ds e stack
    eval' ds (EError s ty) (t:rest) = do
        nt <- eval' ds (EAp ty t) rest
        return (EError s nt)
    eval' ds e@EError {} [] = do return e

    eval' ds e stack= fail . render $ text "Cannot strong:"
                                      <$> pprint e
                                      <$> text "With stack:"
                                      <$> pprint stack
                                      <$> text "And bindings for:"
                                      <$> pprint (Map.keys ds)

    unwind ds t [] = return t
    unwind ds t (t1:rest) = do
        e <-  eval' ds t1 []
        unwind ds (EAp t $ e) rest

    -- currently we do not do eta check. etas should only appear for good reason.
    check_eta x = return x

