{-# LANGUAGE ViewPatterns #-}
module FrontEnd.Lex.Layout(doLayout) where

import Control.Applicative
import Control.Monad.Reader
import Data.Char(toUpper)
import FrontEnd.Lex.Lexer
import FrontEnd.SrcLoc
import PackedString

doLayout :: (Applicative m,Monad m) => FilePath -> [Lexeme] -> m [Lexeme]
doLayout fn ts = layout $ preprocessLexemes fn ts

-- This inserts annotations for newlines that may be used by the layout
-- generator, virtual open braces where needed and removes pragmas we do not
-- understand that may interfere with layout. and propegates LINE pragma
-- annotations as needed

preprocessLexemes :: FilePath -> [Lexeme] -> [Token Lexeme]
preprocessLexemes fn xs = toplevel xs where
    defaultSrcLoc = SrcLoc { srcLocFileName = packString fn, srcLocColumn = 0, srcLocLine = 0 }
    toplevel ls = case skipComments ls of
        (L _ LReservedId "module":_) -> g defaultSrcLoc defaultSrcLoc 0 ls
        (L sl  _ _:_) -> TokenVLCurly "main" (srcLocColumn sl):g defaultSrcLoc defaultSrcLoc 0 ls
        [] -> []
    g floc nloc n ls = f n ls where
        token (L cloc x y) = Token (L (srcLocRelative floc nloc cloc) x y)
        f n ((pragma "LINE") -> (Just lp,rs)) =  case lp of
            [(L _ _ "default")] -> g defaultSrcLoc defaultSrcLoc n rs
            [(L nloc LInteger num),L _ LString s] -> g SrcLoc { srcLocFileName = packString (read s), srcLocLine = read num, srcLocColumn = 0 } nloc { srcLocColumn = 0 } n rs
        f n ((pragma "LINE") -> (Just [(fromL -> "default")],rs)) =  g defaultSrcLoc defaultSrcLoc n rs
        f n (L _ _ "{-#":rs) = f n (d rs) where
            d (L _ _ "#-}":rs) = rs
            d (_:rs) = d rs
            d [] = error "unterminated pragma"
        f n rs@(L sl@SrcLoc { .. } _ _:_) | n /= srcLocLine =
            TokenNL srcLocColumn:f srcLocLine rs
        f n (ls@(L _ _ s):rs@(L sl _ nv:_)) = if s `elem` layoutStarters && nv /= "{"
            then token ls:TokenVLCurly s (srcLocColumn sl):f n rs else token ls:f n rs
        f n (r:rs) = token r:f n rs
        f _ [] = []

fromL (L _ _ x) = x
pragma s ((fromL -> "{-#"):(fromL -> n):ls) | map toUpper n == s = d [] ls  where
    d cs (L _ _ "#-}":rs) = (Just $ reverse cs,rs)
    d cs (r:rs) = d (r:cs) rs
    d cs [] = (Just (reverse cs),[])
pragma _ ls = (Nothing,ls)

layout :: (Applicative m,Monad m) => [Token Lexeme] -> m [Lexeme]
layout ls = runReaderT (g ls []) bogusASrcLoc where
    g ts@(Token (L sl _ _):rs) ctx = local (const sl) $ f ts ctx
    g ts ctx = f ts ctx
    f (TokenNL n:(Token inn@(L _ _ "in")):rs) (Layout "let" n':ls) =
        ([rbrace inn,inn] ++) <$> g rs ls
--    f (TokenNL n:Token s:rs) (Layout h n':ls)
--        | s `elem` layoutContinuers = layout (Token s:rs) (Layout h (min n' n):ls)
    f (TokenNL n:rs@(Token r:_)) ctx@(Layout _ n':ls)
        | n == n' = (semi r:) <$> g rs ctx
        | n > n' = f rs ctx
        | n < n' = (rbrace r:) <$> g (TokenNL n:rs) ls
    f (TokenNL _:rs) ls = g rs ls
    f (TokenVLCurly h n:rs) (Layout h' n':ls)
        | n > n' = mcons lbrace' (g rs (Layout h n:Layout h' n':ls))
        | otherwise = mcons3 lbrace' rbrace' (g rs (Layout h' n':ls))
    f (TokenVLCurly h n:rs) ls = mcons lbrace' (g rs (Layout h n:ls))
    f ((Token t@(L _ _ s)):rs) (dropLayouts -> (n,Just (b,e),ls)) | s == e = do
        rb <- rbrace'
        liftA2 (++) (return $ replicate n rb ++ [t]) (g rs ls)
    f ((Token t@(L _ _ s)):rs) ls | Just e <- lookup s layoutBrackets
        = (t:) `fmap` g rs (NoLayout s e:ls)
    f ((Token t@(L _ _ s)):rs) ls@(Layout c _:_) |
        Just e <- lookup c conditionalBrackets >>= lookup s = (t:) `fmap` g rs (NoLayout s e:ls)
    f ((Token t@(L _ _ "in")):rs) ls = case ls of
        Layout "let" n:ls -> mcons3 rbrace' (return t) (g rs ls)
        ls -> (t:) `fmap` g rs ls
    f (t@(Token (L _ _ ",")):rs) (Layout "let" _:NoLayout "|" e:ls) = rbrace' `mcons` g (t:rs) (NoLayout "|" e:ls)
    f ((Token t@(L _ _ "where")):rs) ls = case ls of
        Layout l n : rest | l `elem` ["do","of"]
            -> mcons3 rbrace' (return t) (g rs rest) -- 'where' closes 'do' and 'case' on equal indentation.
        _otherwise -> (t:) `fmap` g rs ls
    f (Token t:rs) ls = return t `mcons` f rs ls
    f [] (Layout _ n:ls) = liftA2 (:) rbrace' (f [] ls)
    f [] [] = return []
    f x y = error $ "unexpected layout: " ++ show (x,y)
    rbrace (L sl _ _) = L sl LSpecial "}"
    lbrace (L sl _ _) = L sl LSpecial "{"
    semi (L sl _ _) = L sl LSpecial ";"

    mcons = liftA2 (:)
    mcons3 x y zs = liftA2 (:) x (liftA2 (:) y zs)
    rbrace' = ask >>= \sl -> return $ L sl LSpecial "}"
    lbrace' = ask >>= \sl -> return $ L sl LSpecial "{"
    --semi'   = ask >>= \sl -> return $ L sl LSpecial ";"

skipComments :: [Lexeme] -> [Lexeme]
skipComments ls = f ls where
    f (L _ _ "{-#":rs) = f (d rs) where
        d (L _ _ "#-}":rs) = rs
        d (_:rs) = d rs
        d [] = error "unterminated pragma"
    f (l:ls) = l:f ls
    f [] = []

-- VLCurly is only inserted to get a column number of the first lexeme after
-- a layout starter since we don't keep full positions of every lexeme in Token
-- for clarity.
data Token t =
    Token t
    | TokenVLCurly String !Int
    | TokenNL !Int
    deriving(Show)

showToken (Token (L _ _ s)) = s
showToken TokenNL {} = "\n"
showToken TokenVLCurly {} = "{"

data Context
    = NoLayout String String  -- what opened it and what we expect to close it.
    | Layout String !Int
    deriving(Show)

dropLayouts :: [Context] -> (Int,Maybe (String,String),[Context])
dropLayouts cs = f 0 cs where
    f n [] = (n,Nothing,[])
    f n (NoLayout b e:ls) = (n,Just (b,e),ls)
    f n (Layout {}:ls) = f (n + 1) ls

    {-

layout :: [Token] -> [Context] -> [Token]
layout (TokenNL n:Token "in":rs) (Layout "let" n':ls) = rbrace:Token "in":layout rs ls
layout (TokenNL n:Token s:rs) (Layout h n':ls)
    | s `elem` layoutContinuers = layout (Token s:rs) (Layout h (min n' n):ls)
layout (TokenNL n:rs) (Layout h n':ls)
    | n == n' = semi:layout rs (Layout h n':ls)
    | n > n' = layout rs (Layout h n':ls)
    | n < n' = rbrace:layout (TokenNL n:rs) ls
layout (TokenNL _:rs) ls = layout rs ls
layout (TokenVLCurly h n:rs) (Layout h' n':ls)
    | n > n' = lbrace:layout rs (Layout h n:Layout h' n':ls)
    | otherwise = lbrace : rbrace : layout rs (Layout h' n':ls)
layout (TokenVLCurly h n:rs) ls = lbrace:layout rs (Layout h n:ls)
layout (t@(Token s):rs) (dropLayouts -> (n,Just (b,e),ls)) | s == e
    = replicate n rbrace ++ t:layout rs ls
layout (t@(Token s):rs) ls | Just e <- lookup s layoutBrackets
    = t:layout rs (NoLayout s e:ls)
layout (t@(Token s):rs) ls@(Layout c _:_) |
    Just e <- lookup c conditionalBrackets >>= lookup s = t:layout rs (NoLayout s e:ls)
layout (t@(Token "in"):rs) ls = case ls of
    Layout "let" n:ls -> rbrace:t:layout rs ls
    ls -> t:layout rs ls
layout (t@(Token ","):rs) (Layout "let" _:NoLayout "|" e:ls) = rbrace:layout (t:rs) (NoLayout "|" e:ls)
layout (t@(Token "where"):rs) ls = case ls of
    Layout l n : rest | l `elem` ["do","of"]
        -> rbrace : t : layout rs rest -- 'where' closes 'do' and 'case' on equal indentation.
    _otherwise -> t : layout rs ls

layout (t:rs) ls = t:layout rs ls
layout [] (Layout _ n:ls) = rbrace:layout [] ls
layout [] [] = []
layout x y = error $ "unexpected layout: " ++ show (x,y)

-- unwind all pending layouts
dropLayouts :: [Context] -> (Int,Maybe (String,String),[Context])
dropLayouts cs = f 0 cs where
    f n [] = (n,Nothing,[])
    f n (NoLayout b e:ls) = (n,Just (b,e),ls)
    f n (Layout {}:ls) = f (n + 1) ls

semi = Token ";"
lbrace = Token "{"
rbrace = Token "}"
-}

fsts = map fst
snds = map snd

layoutStarters   = ["where","let","of","do"]

-- these symbols will never close a layout.
layoutContinuers = ["|","->","=",";",","]

-- valid in all contexts
layoutBrackets = [
    ("case","of"),
    ("if","then"),
    ("then","else"),
    ("(",")"),
    ("[","]"),
    ("{","}")
    ]

conditionalBrackets = [
    ("of",[("|","->")]),
    ("let",[("|","=")]),
    ("[",[("|","]")])
    ]
