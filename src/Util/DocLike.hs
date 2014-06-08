module Util.DocLike(module Util.DocLike, module Data.Monoid) where

-- simplified from Doc.DocLike

import Control.Applicative
import Data.Monoid(Monoid(..),(<>))
import Data.Traversable as T
import qualified Text.PrettyPrint.HughesPJ as P

--infixr 5 <$> -- ,<//>,<$>,<$$>
infixr 6 <+>, <->
infixl 5 $$, $+$

-- we expect a monoid instance with <>
class DocLike a where
    emptyDoc :: a
    text :: String -> a
    oobText :: String -> a
    char :: Char -> a
    char x = text [x]
    (<+>) :: a -> a -> a
    (<->) :: a -> a -> a
    ($$) :: a -> a -> a
    ($+$) :: a -> a -> a
    hsep :: [a] -> a
    hcat :: [a] -> a
    vcat :: [a] -> a
    tupled :: [a] -> a
    list :: [a] -> a
    fsep :: [a] -> a
    fcat :: [a] -> a
    sep :: [a] -> a
    cat :: [a] -> a
    semiBraces :: [a] -> a
    enclose :: a -> a -> a -> a
    encloseSep :: a -> a -> a -> [a] -> a

    oobText _ = emptyDoc
    emptyDoc = text []
    hcat [] = emptyDoc
    hcat xs = foldr1 (<->) xs
    hsep [] = emptyDoc
    hsep xs = foldr1 (<+>) xs
    vcat [] = emptyDoc
    vcat xs = foldr1 (\x y -> x <-> char '\n' <-> y) xs
    fsep = hsep
    fcat = hcat
    sep = hsep
    cat = hcat
    x <+> y = x <-> char ' ' <-> y
    x $$ y = x <-> char '\n' <-> y
    x $+$ y = x $$ y
    encloseSep l r s ds = enclose l r (hcat $ punctuate s ds)
    enclose l r x   = l <-> x <-> r
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
    :: DocLike a => a
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
punctuate p (d:ds)  = (d <-> p) : punctuate p ds

newtype ShowSDoc = SD { unSD :: String -> String }
showSD (SD s) = s ""

instance Monoid ShowSDoc where
    mempty = SD id
    mappend (SD a) (SD b) = SD $ a . b

instance (DocLike ShowSDoc) where
    char c = SD (c:)
    text s = SD (s ++)
    SD x <+> SD y = SD $ x . (' ':) . y
    x <-> y = mappend x y
    emptyDoc = mempty

instance (DocLike [Char]) where
    char c = [c]
    text s = s
    x <+> y = x ++ " " ++ y
    x <-> y = mappend x y
    emptyDoc = mempty

instance (DocLike a, Applicative m) => DocLike (m a) where
    emptyDoc = pure emptyDoc
    char x = pure (char x)
    text x = pure (text x)
    ($$) = liftA2 ($$)
    ($+$) = liftA2 ($+$)
    (<+>) = liftA2 (<+>)
    (<->) = liftA2 (<->)
    vcat xs = vcat <$> traverse id xs
    hsep xs = hsep <$> traverse id xs

---------------------
-- HughesPJ instances
---------------------

-- instance Monoid P.Doc where
--     mappend = (P.<>)
--     mempty = P.empty
--     mconcat = P.hcat

instance DocLike P.Doc where
    emptyDoc = mempty
    text = P.text
    char = P.char
    (<->) = (P.<>)
    (<+>) = (P.<+>)
    ($$) = (P.$$)
    ($+$) = (P.$+$)
    hsep = P.hsep
    vcat = P.vcat
    oobText = P.zeroWidthText
    fcat = P.fcat
    fsep = P.fsep
    cat = P.cat
    sep = P.sep
