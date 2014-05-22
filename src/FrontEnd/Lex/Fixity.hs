module FrontEnd.Lex.Fixity where

{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
import Control.Monad
import Control.Monad.Identity
import Data.Maybe
import Data.Tree
import System.Environment
import Text.Show

data Fixity = L | R | N | Prefix | Postfix
    deriving(Show,Eq)

type Prec = (Fixity,Int)

-- alternating operators and expressions, always succeeds.

simpleShunt :: (Show e, Show o)
    => (o -> Prec)
    -> e -> [(o,e)]
    -> (o -> e -> e -> e) -> e

simpleShunt getPrec e oes combine = runIdentity $ do
    let ShuntSpec { .. } = shuntSpec
    let lookupToken (Left e) = return $ Left e
        lookupToken (Right o) = return $ Right (getPrec o)
        operator (Right t) [x,y] = return $ combine t x y
        ss = ShuntSpec { .. }
    shunt ss (Left e:concat [ [Right o, Left e] | (o,e) <- oes])

data S t e = S {
    stack :: [(Maybe t,Prec)],
    output :: [e],
    lastWasOp :: Bool
    } deriving(Show)
initialState = S { stack = [], output = [], lastWasOp = True }

-- lookups must be idempotent.
data ShuntSpec m t e = ShuntSpec {
    appPrec     :: Prec,                   -- Precedence of application.
    lookupToken :: t -> m (Either e Prec), -- A token is either an operator or an expression.
    lookupUnary :: t -> m (Maybe Int),     -- What to do when an unexpected op happens, either can be interpeted as unary or fail.
    application :: e -> e -> m e,          -- what to do on application
    operator    :: t -> [e] -> m e,        -- what to do on operator
    emptyInput  :: m e,                    -- what to do on empty input
    trailingOps :: e -> t -> m e,          -- what to do with trailing operators
    equalFixity :: Prec -> [Maybe t] -> m e-- what to do on nonassociate fixities
}

shuntSpec :: (Monad m, Show t) => ShuntSpec m t e
shuntSpec = ShuntSpec { .. } where
    appPrec          = (L,10)
    lookupToken t    = fail $ "ShuntSpec.lookupToken unspecified: " ++ show t
    lookupUnary t    = fail $ "Operator used in unary location: " ++ show t
    application _ _  = fail $ "ShuntSpec.application unspecified"
    operator t _     = fail $ "ShuntSpec.operator unspecified: " ++ show t
    emptyInput       = fail $ "Empty input to precedence parser"
    trailingOps _ ts = fail $ "Trailing operators: " ++ show ts
    equalFixity p ts = fail $ "Cannot mix operators of same fixity: "
        ++ show p ++ " " ++ show ts

shunt :: (Show t, Show e,Monad m)
    => ShuntSpec m t e   -- specification
    -> [t]               -- token stream
    -> m e

shunt ShuntSpec { .. } xs = f initialState xs where
    f S { output = [e], stack = [] } [] = return e
    f S { output = [], stack = [] } [] = emptyInput

    f s@S { .. } (t:ts) = lookupToken t >>= \x -> case (x,lastWasOp) of
        (Left o,True) ->  f s { output = o:output, lastWasOp = False } ts
        (Left o,False) -> op s { lastWasOp = True } (Nothing,appPrec) Nothing (t:ts)
        (Right p@(Prefix,_),False) -> op s { lastWasOp = True } (Nothing,appPrec) Nothing (t:ts)
        (Right p,False) -> op s { lastWasOp = True } (Just t,p) Nothing ts
        (Right p@(Prefix,_),True) -> op s  (Just t,p) Nothing ts
        (Right p@(_,n),True) -> do
            mn <- lookupUnary t
            let Just n' = mplus mn (Just n)
            op s (Just t,(Prefix,n')) Nothing ts

    f s@S { output = (x:os) , stack = (t,(Prefix,_)):ss } [] = do
        ne <- apop t [x]
        f s { output = ne:os, stack = ss } []
    f s@S { output = (x:y:os) , stack = (t,_):ss } [] = do
        ne <- apop t [y,x]
        f s { output = ne:os, stack = ss } []

    f s@S { output = [e], stack = (Just t,_):ss } [] = do
        ne <- trailingOps e t -- (catMaybes $ map fst stack)
        f s { output = [ne], stack = ss } []
    f s [] = error $ show s

    op s@S { .. } o@(_,prec) me ts = g s where
        g s@S { stack = [] } = f (addOut s { stack = [o] }) ts
        g s@S { stack = (t,prec'@(Prefix,_)):ss, output = (x:rs) }
            | prec' `gt` prec = do
                ne <- apop t [x]
                g s { stack = ss, output = ne:rs }
        g s@S { stack = (t,prec'@(N,_)):ss, output = (x:y:rs) }
            | prec' == prec = do equalFixity prec [t]
        g s@S { stack = (t,prec'):ss, output = (x:y:rs) }
            | prec' `gt` prec = do
                ne <- apop t [y,x]
                g s { stack = ss, output = ne:rs }
        g s@S { .. } = f (addOut s { stack = o:stack }) ts
        addOut s@S { ..} = case me of
            Just e -> s { output = e:output }
            Nothing -> s
        adj L n = n - 1
        adj _ n = n
    apop t [x,y] = case t of
        Nothing -> application x y
        Just t' -> operator t' [x,y]
    apop t xs = operator t' xs where
        Just t' = t

    _ `gt` (Prefix,_) = False
    (Prefix,x) `gt` (_,y) = x >= y
    (L,x) `gt` (_,y) = x >= y
    (_,x) `gt` (_,y) = x > y

    sentinel = (R,-1)

precs = procPrecs
    [prec Prefix ["~"]
    ,prec R ["^"]
    ,prec L ["/","*","%"]
    ,prec R []
    ,prec L ["+","-"]
    ,prec L ["."]
    ,prec N ["=~"]
    ,prec Prefix ["!"]
    ,prec N ["<","<=",">=","!=","=="]
    ,prec L ["&&"]
    ,prec L ["||"]
    ]
prec p ws = (p,ws)
procPrecs ps = concatMap f (zip (reverse ps) [0 :: Int ..]) where
    f ((p,ws),n) = [ (w,(p,n)) | w <- ws ]

main = do
    xs <- getArgs
    let ts = concatMap words xs
        ShuntSpec { .. } = shuntSpec
    let lookupToken x = return $ case lookup x precs of
            Just y -> Right y
            _ -> Left x
        operator t [x,y] = return $ parens (x <+> t <+> y)
        operator t [x] = return $ brackets (t <+> x)
        application = \x y -> return $ parens (x <+> y)
        lookupUnary = \_ -> return Nothing
        --trailingOps e t = return $ "<" ++ e ++ " " ++ t ++ ">"
        appPrec = (L,7)
    res <- shunt ShuntSpec { .. } ts
    r2 <- shunt (treeShuntSpec "" (return . (`lookup` precs))) { lookupUnary, appPrec } ts
--    mapM_ print precs
    putStrLn res
    putStrLn $ drawParenTree r2

x <+> y = x ++ " " ++ y
parens x = "(" ++ x ++ ")"
brackets x = "[" ++ x ++ "]"

treeShuntSpec :: (Monad m, Show t)
    => t                     -- token value to use for applications.
    -> (t -> m (Maybe Prec)) -- lookup precedence
    -> ShuntSpec m t (Tree t)
treeShuntSpec app lup = shuntSpec { application, operator, lookupToken } where
    application x y = return $ Node app [x,y]
    operator t xs = return $ Node t xs
    lookupToken t = lup t >>= \x -> case x of
        Just p -> return $ Right p
        Nothing -> return $ Left (Node t [])

drawParenTree :: Tree String -> String
drawParenTree t = f t [] where
    f (Node t []) = showString t
    f (Node "" [x,y]) = showChar '(' . f x . showChar ' ' . f y . showChar ')'
    f (Node t [x,y]) = showChar '(' . f x . showChar ' ' . showString t . showChar ' ' . f y . showChar ')'
    f (Node t [x]) = showChar '(' . showString t . showChar ' ' . f x . showChar ')'
    f (Node t xs) = showString t . showListWith id (map f xs)
