module E.CPR(Val(..), cprAnalyzeDs) where

import Control.Monad.Writer hiding(Product(..))
import Data.Typeable
import Data.Monoid()
import qualified Data.Map as Map

import Binary
import DataConstructors
import Doc.DocLike
import E.E
import E.FreeVars
import GenUtil
import Name.Name
import Name.Names
import Number
import Util.SameShape
import qualified Doc.Chars as C
import qualified E.Demand as Demand
import qualified Info.Info as Info

newtype Env = Env (Map.Map TVr Val)
    deriving(Monoid)

data Val =
    Top               -- the top.
    | Fun Val         -- function taking an arg
    | Tup Name [Val]  -- A constructed product
    | VInt Number     -- A number
    | Tag [Name]      -- A nullary constructor, like True, False
    | Bot             -- the bottom
    deriving(Eq,Ord,Typeable)
    {-! derive: GhcBinary !-}

toVal c = case conSlots c of
    [] -> Tag [conName c]
    ss -> Tup (conName c) [ Top | _ <- ss]


instance Show Val where
    showsPrec _ Top = C.top
    showsPrec _ Bot = C.bot
    showsPrec n (Fun v) = C.lambda <> showsPrec n v
    showsPrec _ (Tup n xs) = shows n <> tupled (map shows xs)
    showsPrec _ (VInt n) = shows n
    showsPrec _ (Tag ns) = shows ns

lub :: Val -> Val -> Val
lub Bot a = a
lub a Bot = a
lub Top a = Top
lub a Top = Top
lub (Tup a xs) (Tup b ys)
    | a == b, sameShape1 xs ys = Tup a (zipWith lub xs ys)
    | a == b = error "CPR.lub this shouldn't happen"
    | otherwise = Top
lub (Fun l) (Fun r) = Fun (lub l r)
lub (VInt n) (VInt n') | n == n' = VInt n
lub (Tag xs) (Tag ys) = Tag (smerge xs ys)
lub (Tag _) (Tup _ _) = Top
lub (Tup _ _) (Tag _) = Top
lub _ _ = Top
--lub a b = error $ "CPR.lub: " ++ show (a,b)


instance Monoid Val where
    mempty = Bot
    mappend = lub

cprAnalyzeDs :: DataTable -> [(TVr,E)] -> [(TVr,E)]
cprAnalyzeDs dataTable ds = fst $ cprAnalyzeBinds dataTable mempty ds


cprAnalyzeBinds :: DataTable -> Env -> [(TVr,E)] -> ([(TVr,E)],Env)
cprAnalyzeBinds dataTable env bs = f env  (decomposeDefns bs) [] where
    f env (Left (t,e):rs) zs = case cprAnalyze dataTable env e of
        (e',v) -> f (envInsert t v env) rs ((tvrInfo_u (Info.insert v) t,e'):zs)
    f env (Right xs:rs) zs = g (length xs + 2) ([ (t,(e,Bot)) | (t,e) <- xs]) where
        g 0 mp =  f nenv rs ([ (tvrInfo_u (Info.insert b) t,e)   | (t,(e,b)) <- mp] ++ zs)  where
            nenv = Env (Map.fromList [ (t,b) | (t,(e,b)) <- mp]) `mappend` env
        g n mp = g (n - 1) [ (t,cprAnalyze dataTable nenv e)  | (t,e) <- xs] where
            nenv = Env (Map.fromList [ (t,b) | (t,(e,b)) <- mp]) `mappend` env
    f env [] zs = (reverse zs,env)


envInsert :: TVr -> Val -> Env -> Env
envInsert tvr val (Env mp) = Env $ Map.insert tvr val mp

cprAnalyze :: DataTable -> Env -> E -> (E,Val)
cprAnalyze dataTable env e = cprAnalyze' env e where
    cprAnalyze' (Env mp) (EVar v)
        | Just t <- Map.lookup v mp = (EVar v,t)
        | Just t <- Info.lookup (tvrInfo v)  = (EVar v,t)
        | otherwise = (EVar v,Top)
    cprAnalyze' env ELetRec { eDefs = ds, eBody = e } = (ELetRec ds' e',val) where
        (ds',env') = cprAnalyzeBinds dataTable env ds
        (e',val) = cprAnalyze' (env' `mappend` env) e

    cprAnalyze' env (ELam t e)
        | Just (Demand.S _) <- Info.lookup (tvrInfo t), Just c <- getProduct dataTable (tvrType t) = let
            (e',val) = cprAnalyze' (envInsert t (toVal c) env) e
            in (ELam t e',Fun val)
    cprAnalyze' env (ELam t e) = (ELam t e',Fun val) where
        (e',val) = cprAnalyze' (envInsert t Top env) e
    cprAnalyze' env ec@(ECase {}) = runWriter (caseBodiesMapM f ec) where
        f e = do
            (e',v) <- return $ cprAnalyze' env e
            tell v
            return e'
    cprAnalyze' env (EAp fun arg) = (EAp fun_cpr arg,res_res) where
        (fun_cpr, fun_res) = cprAnalyze' env fun
        res_res = case fun_res of
            Fun x -> x
            Top -> Top
            Bot -> Bot
            v -> error $ "cprAnalyze'.res_res: " ++ show v
    cprAnalyze' env  e = (e,f e) where
        f (ELit (LitInt n _)) = VInt n
        f (ELit LitCons { litName = n, litArgs = [], litType = _ }) = Tag [n]
        f (ELit LitCons { litName = n, litArgs = xs, litType = _ }) = Tup n (map g xs)
        f (EPi t e) = Tup tc_Arrow [g $ tvrType t, g e]
        f (EPrim {}) = Top -- TODO fix primitives
        f (EError {}) = Bot
        f e = error $ "cprAnalyze'.f: " ++ show e
        g = snd . cprAnalyze' env




