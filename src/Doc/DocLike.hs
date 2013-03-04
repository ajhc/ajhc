{-# LANGUAGE CPP,UndecidableInstances,OverlappingInstances #-}
module Doc.DocLike where

#include "hs_src_config.h"

-- arch-tag: a88f19fb-e18d-475f-b6d1-8da78676261a

import Control.Monad.Reader()
import qualified Text.PrettyPrint.HughesPJ as P

infixr 5 <$> -- ,<//>,<$>,<$$>
infixr 6 <>
infixr 6 <+>

class TextLike a where
    empty :: a
    text :: String -> a
    --string :: String -> a
    char :: Char -> a
    --char '\n' = string "\n"
    char x = text [x]
    empty = text ""

class (TextLike a) => DocLike a where
    (<>) :: a -> a -> a
    (<+>) :: a -> a -> a
    (<$>) :: a -> a -> a
    hsep :: [a] -> a
    hcat :: [a] -> a
    vcat :: [a] -> a
    tupled :: [a] -> a
    list :: [a] -> a
    semiBraces :: [a] -> a
    enclose :: a -> a -> a -> a
    encloseSep :: a -> a -> a -> [a] -> a

    hcat [] = empty
    hcat xs = foldr1 (<>) xs
    hsep [] = empty
    hsep xs = foldr1 (<+>) xs
    vcat [] = empty
    vcat xs = foldr1 (\x y -> x <> char '\n' <> y) xs
    x <+> y = x <> char ' ' <> y
    x <$> y = x <> char '\n' <> y
    encloseSep l r s ds = enclose l r (hcat $ punctuate s ds)
    enclose l r x   = l <> x <> r
    list            = encloseSep lbracket rbracket comma
    tupled          = encloseSep lparen   rparen  comma
    semiBraces      = encloseSep lbrace   rbrace  semi

------------------------
-- Basic building blocks
------------------------

tshow :: (Show a,DocLike b) => a -> b
tshow x = text (show x)

lparen,rparen,langle,rangle,
    lbrace,rbrace,lbracket,rbracket,squote,
    dquote,semi,colon,comma,space,dot,backslash,equals
    :: TextLike a => a
lparen          = char '('
rparen          = char ')'
langle          = char '<'
rangle          = char '>'
lbrace          = char '{'
rbrace          = char '}'
lbracket        = char '['
rbracket        = char ']'

squote          = char '\''
dquote          = char '"'
semi            = char ';'
colon           = char ':'
comma           = char ','
space           = char ' '
dot             = char '.'
backslash       = char '\\'
equals          = char '='

squotes x = enclose squote squote x
dquotes x = enclose dquote dquote x
parens x = enclose lparen rparen x
braces x = enclose lbrace rbrace x
brackets x = enclose lbracket rbracket x
angles x = enclose langle rangle x

-----------------------------------------------------------
-- punctuate p [d1,d2,...,dn] => [d1 <> p,d2 <> p, ... ,dn]
-----------------------------------------------------------
punctuate _ []      = []
punctuate _ [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds

------------------
-- String instance
------------------
instance TextLike String where
    empty = ""
    text x = x

instance TextLike Char where
    empty = error "TextLike: empty char"
    char x = x
    text [ch] = ch
    text _ = error "TextLike: string to char"

instance DocLike String where
    a <> b = a ++ b
    a <+> b = a ++ " " ++ b

instance TextLike ShowS where
    empty = id
    text x = (x ++)
    char c = (c:)

instance DocLike ShowS where
    a <> b = a . b

instance (TextLike a, Monad m) => TextLike (m a) where
    empty = return empty
    char x = return (char x)
    text x = return (text x)

instance (DocLike a, Monad m,TextLike (m a)) => DocLike (m a) where
    a <$> b = do
        a <- a
        b <- b
        return (a <$> b)
    a <> b = do
        a <- a
        b <- b
        return (a <> b)
    a <+> b = do
        a <- a
        b <- b
        return (a <+> b)
    vcat xs = sequence xs >>= return . vcat
    hsep xs = sequence xs >>= return . hsep

---------------------
-- HughesPJ instances
---------------------

instance TextLike P.Doc where
    empty = P.empty
    text = P.text
    char = P.char

#if !HAS_MONOID_DOC
instance Monoid P.Doc where
    mappend = (P.<>)
    mempty = P.empty
    mconcat = P.hcat
#endif

instance DocLike P.Doc where
    (<>) = (P.<>)
    (<+>) = (P.<+>)
    (<$>) = (P.$$)
    hsep = P.hsep
    vcat = P.vcat

    --brackets = P.brackets
    --parens = P.parens

--------
-- simple instances to allow distribution of an environment
--------
--instance Monoid a => Monoid (b -> a) where
--    mempty = \_ -> mempty
--    mappend x y = \a -> mappend (x a) (y a)
--    mconcat xs = \a -> mconcat (map ($ a) xs)
--
--instance (DocLike a, Monoid (b -> a)) => DocLike (b -> a) where
--    parens x = \a -> parens (x a)
--    (<+>) x y = \a -> x a <+> y a
