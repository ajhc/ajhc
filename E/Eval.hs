module E.Eval(eval, unify,strong) where

-- Simple lambda Calculus interpreter
-- does not handle recursive Let or Case statements, but those don't appear in types anyway.

import E.E
import E.Subst
import E.Pretty
import E.FreeVars
import Seq
import Control.Monad.Writer
import qualified Data.Map as Map
import Doc.PPrint



eval :: E -> E
eval term = eval' term []  where
    eval' t@EVar {} [] = t
    eval' (ELam v body) [] = check_eta $ ELam v (eval body)
    eval' (EPi v body) [] = check_eta $ EPi v (eval body)
    eval' e@Unknown [] = e
    eval' e@ESort {} [] = e

    eval' (ELit (LitCons n es t)) [] = ELit $ LitCons n (map eval es) t
    eval' e@ELit {} [] = e
    eval' (ELit (LitCons n es ty)) (t:rest) = eval' (ELit $ LitCons n (es ++ [t]) (eval $ EAp ty t)) rest

    eval' (ELam v body) (t:rest) = eval' (subst v t body) rest
    eval' (EPi v body) (t:rest) = eval' (subst v t body) rest   -- fudge
    eval' (EAp t1 t2) stack = eval' t1 (t2:stack)
    eval' t@EVar {} stack = unwind t stack
    eval' (ELetRec ds e) stack = eval' (f (decomposeDefns ds) e) stack where
        f [] e = e
        f (Left (x,y):ds) e =  subst x y (f ds e)
        f (Right _:_) _ = error $ "cannot eval recursive let"
    eval' e _ = error $ "Cannot eval: " ++ show e

    unwind t [] = t
    unwind t (t1:rest) = unwind (EAp t $ eval t1) rest

    -- currently we do not do eta check. etas should only appear for good reason.
    check_eta x = x



-- TODO, this should take a set of free variables and α-convert lambdas

unify :: Monad m => E -> E -> m [(E,E)]
unify e1 e2 = liftM Seq.toList $ execWriterT (un e1 e2 () (0::Int)) where
    un (EAp a b) (EAp a' b') mm c = do
        un a a' mm c
        un b b' mm c
    un a@(EVar (TVr { tvrIdent = (i), tvrType =  t}))  b@(EVar (TVr { tvrIdent = ( j), tvrType =  u})) mm c = do
        un t u mm c
        when (i /= j) $ tell (Seq.single (a,b))
    --un (ELam (TVr Nothing ta) ea) (ELam (TVr Nothing tb) eb) mm c = un ta tb mm c >> un ea eb mm c
    --un (EPi (TVr Nothing ta) ea) (EPi (TVr Nothing tb) eb) mm c = un ta tb mm c >> un ea eb mm c
    un (ELam va ea) (ELam vb eb) mm c = lam va ea vb eb mm c
    un (EPi va ea) (EPi vb eb) mm c = lam va ea vb eb mm c
    un (EPrim s xs t) (EPrim s' ys t') mm c | length xs == length ys = do
        sequence_ [ un x y mm c | x <- xs | y <- ys]
        un t t' mm c
    un (ESort x) (ESort y) mm c | x == y = return ()
    un (ELit (LitInt x t1))  (ELit (LitInt y t2)) mm c | x == y = un t1 t2 mm c
--    un (ELit (LitChar x))  (ELit (LitChar y)) mm c | x == y = return ()
--    un (ELit (LitFrac x t1 ))  (ELit (LitFrac y t2)) mm c | x == y = un t1 t2 mm c
    un (ELit (LitCons n xs t))  (ELit (LitCons n' ys t')) mm c | n == n' && length xs == length ys = do
        sequence_ [ un x y mm c | x <- xs | y <- ys]
        un t t' mm c
    un a@EVar {} b _ _ = tell (Seq.single (a,b))
    --un a b@EVar {} _ _ = tell (Seq.single (a,b))
    un a b _ _ = fail $ "Expressions do not unify: " ++ show a ++ show b
    lam va ea vb eb mm c = do
        un ea eb mm c

    -- error "cannot handle lambdas yet"



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
    --eval' ds (ELetRec ds' e) = (Map.fromList ds'
    eval' ds (ELit (LitCons n es t)) [] = do
        es' <- mapM (\e -> eval' ds e []) es
        t' <-  (eval' ds t [])
        return $ ELit $ LitCons n es' t'
    eval' ds e@ELit {} [] = return e
    eval' ds (ELit (LitCons n es ty)) (t:rest) = do
        t' <- (eval' ds (EAp ty t) [])
        eval' ds (ELit $ LitCons n (es ++ [t]) t') rest
    eval' ds (ELam v body) (t:rest) = eval' ds (subst v t body) rest
    eval' ds (EPi v body) (t:rest) = eval' ds (subst v t body) rest   -- fudge
    eval' ds (EAp t1 t2) stack = eval' ds t1 (t2:stack)
    eval' ds t@(EVar v) stack
        | Just x <- Map.lookup v ds = eval' ds x stack
        | otherwise = do
            tvr <- etvr ds v
            unwind ds (EVar tvr) stack
    eval' ds (ELetRec ds' e) stack = eval' (Map.fromList ds'  `mappend` ds) e  stack
    --eval' ds (ELetRec ds' e) stack = eval' ds (f (decomposeDefns ds') e) stack where
    --    f [] e = e
    --    f (Left (x,y):ds) e =  subst x y (f ds e)
    --    f (Right _:_) _ = error $ "cannot eval recursive let"
--    eval' ds (ECase e as) [] = do
--        e' <- eval' ds e []
--        let f (PatLit (LitCons n es t),e) = do
--                e' <- eval' ds e []
--                t' <- eval' ds t []
--                es' <- mapM (\e -> eval' ds e []) es
--                return (PatLit (LitCons n es' t'),e')
--            f (p,e) = do
--                e' <- eval' ds e []
--                return (p,e')
--        as' <- mapM f as
--        return $ ECase e' as'

    eval' ds e stack= fail $ "Cannot strong: \n" ++ render (pprint (e,stack))

    unwind ds t [] = return t
    unwind ds t (t1:rest) = do
        e <-  eval' ds t1 []
        unwind ds (EAp t $ e) rest

    -- currently we do not do eta check. etas should only appear for good reason.
    check_eta x = return x

