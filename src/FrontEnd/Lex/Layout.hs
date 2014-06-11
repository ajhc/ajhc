{-# LANGUAGE ViewPatterns #-}
module FrontEnd.Lex.Layout(preprocessLexemes,Token(..)) where

import Data.Char(toUpper,ord)
import FrontEnd.Lex.Lexer
import FrontEnd.SrcLoc
import Options
import PackedString
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified FlagOpts as FO

-- Carry out various transformations on the lexeme stream before sending off to
-- the parser.
--
-- This inserts annotations for newlines that may be used by the layout
-- generator, virtual open braces where needed and removes pragmas we do not
-- understand that may interfere with layout.
--
-- It handles LINE pragmas propegating their values to the pending srclocs.
--
-- replaces (op) and `var` with their corresponding duals.
--
-- translate unicode symbols to their traditional counterparts
--
-- turn reserved ids into regular ids if the associated extension is disabled

-- VLCurly is only inserted to get a column number of the first lexeme after
-- a layout starter since we don't keep full positions of every lexeme in Token
-- for clarity.
data Token t =
    Token t
    | TokenVLCurly t !Int
    | TokenNL !Int
    deriving(Show)

preprocessLexemes :: Opt -> FilePath -> [Lexeme] -> [Token Lexeme]
preprocessLexemes opt fn xs = fixupLets $ toplevel xs where
    defaultSrcLoc = SrcLoc { srcLocFileName = packString fn, srcLocColumn = 0, srcLocLine = 0 }
    toplevel ls = case skipComments ls of
        (L _ LReservedId "module":_) -> g defaultSrcLoc defaultSrcLoc 0 ls
        (L _ _ "{":_) -> g defaultSrcLoc defaultSrcLoc 0 ls
        (L sl  _ _:_) -> TokenVLCurly (L sl LReservedId "where") (srcLocColumn sl):g defaultSrcLoc defaultSrcLoc 0 ls
        [] -> [TokenVLCurly (L defaultSrcLoc LReservedId "where") 0]
    g floc nloc n ls = f n ls where
        token (L cloc x y) = Token (L (srcLocRelative floc nloc cloc) x y)

        f n (L sl LQReservedId (splitName -> (conq, con, ri)):rs) = ans where
            ans = f n (
                L sl (if conq then LQConId else LConId) con:
                L (adv (length con) sl) LVarSym ".":
                L (adv (length con + 1) sl) LReservedId ri:rs)
            adv n sl = srcLocColumn_u (n +) sl

        f n (L sl LSpecial s:rs) | Just flag <- Map.lookup s reservedMap, not (fopts' opt flag) = f n (L sl LVarId s:rs)
        f n (L sl LReservedId s:rs) | Just flag <- Map.lookup s reservedMap, not (fopts' opt flag) = f n (L sl LVarId s:rs)
        f n (L sl LSpecial [c]:rs) | Just (lc,v) <- IM.lookup (ord c) uniMap = f n (L sl lc v:rs)
        f n (L sl _ "\\":L _ _ "case":rs) = f n (L sl LSpecial "\\case":rs)
        f n (L sl _ "(":L _ LVarSym z:L _ _ ")":rs) = f n (L sl LVarId z:rs)
        f n (L sl _ "(":L _ LConSym z:L _ _ ")":rs) = f n (L sl LConId z:rs)
        f n (L sl _ "`":L _ LVarId z:L _ _ "`":rs) = f n (L sl LVarSym z:rs)
        f n (L sl _ "`":L _ LConId z:L _ _ "`":rs) = f n (L sl LConSym z:rs)
        f n (L sl _ "(":L _ LQVarSym z:L _ _ ")":rs) = f n (L sl LQVarId z:rs)
        f n (L sl _ "(":L _ LQConSym z:L _ _ ")":rs) = f n (L sl LQConId z:rs)
        f n (L sl _ "`":L _ LQVarId z:L _ _ "`":rs) = f n (L sl LQVarSym z:rs)
        f n (L sl _ "`":L _ LQConId z:L _ _ "`":rs) = f n (L sl LQConSym z:rs)
        f n ((pragma "LINE") -> (Just lp,rs)) =  case lp of
            [(L _ _ "default")] -> g defaultSrcLoc defaultSrcLoc n rs
            [(L nloc LInteger num),L _ LString s] -> g SrcLoc {
                srcLocFileName = packString (read s), srcLocLine = read num - 1, srcLocColumn = 0 }
                    nloc { srcLocColumn = 0 } n rs
            [(L nloc LInteger num)] -> g floc {srcLocLine = read num - 1, srcLocColumn = 0 }
                    nloc { srcLocColumn = 0 } n rs
            _ -> f n rs
        f n (x@(fromL -> "{-#"):(fromL -> "JHC"):xs) = f n (x:xs)
        f n (L sl _ "{-#":(fromL -> pn):xs) | Just npn <- Map.lookup pn pragmaMap = f n (L sl LPragmaStart npn:xs)
        f n (L sl _ "{-#":(fromL -> 'J':'H':'C':'_':pn):xs) | Just npn <- Map.lookup pn pragmaMap = f n (L sl LPragmaStart npn:xs)
        f n (L _ _ "{-#":rs) = f n (d rs) where
            d (L _ _ "#-}":rs) = rs
            d (_:rs) = d rs
            d [] = []
        f n rs@(L sl@SrcLoc { .. } _ _:_) | n /= srcLocLine =
            TokenNL srcLocColumn:f srcLocLine rs
        f n (ls@(L _ _ s):rs@(skipComments -> L sl _ nv:_)) = if s `elem` layoutStarters && nv /= "{"
            then token ls:TokenVLCurly ls (srcLocColumn sl):f n rs else token ls:f n rs
        f n [ls@(L _ _ s)] | s `elem` layoutStarters = token ls:TokenVLCurly ls 0:[]
        f n (r:rs) = token r:f n rs
        f _ [] = []

        pragmaMap = Map.fromList $ [ (y,x) | xs@(x:_) <- pragmaKeepMap, y <- xs]
            ++ [ (x,x) | x <- pragmaKeep ]
        reservedMap = Map.fromList
            [("foreign", FO.Ffi), ("forall", FO.Forall), ("exists", FO.Exists)
            ,("kind", FO.UserKinds), ("family", FO.TypeFamilies), ("alias", FO.Never)
            ,("prefixx", FO.Never), ("prefixy", FO.Never), ("closed", FO.Never)
            ]
        pragmaKeep = ["NOETA", "SUPERINLINE", "CTYPE", "INLINE", "HOT", "COLD"]
        pragmaKeepMap =
            [["NOINLINE", "NOTINLINE"]
            ,["CATALYST", "CATALYSTS"]
            ,["SPECIALIZE", "SPECIALISE"]
            ,["MULTISPECIALIZE", "MULTISPECIALISE"]
            ,["SUPERSPECIALIZE", "SUPERSPECIALISE"]
            ,["RULE","RULES", "JHC_RULE", "JHC_RULES", "RULES_JHC"]]

-- TODO: this double reverses the lexeme list, we should deforest this with the
-- above preprocessing routine.
fixupLets :: [Token Lexeme] -> [Token Lexeme]
fixupLets xs = f (0::Int) (reverse xs) [] where
    f 0 (Token (L sl nt "let"):xs) rs =  f 0 xs (Token (L sl nt "let#"):rs)
    f n (x@(Token (L _ _ "let")):xs) rs =  f (n - 1) xs (x:rs)
    f n (x@(Token (L _ _ "in")):xs) rs =  f (n + 1) xs (x:rs)
    f n (x:xs) rs = f n xs (x:rs)
    f n [] rs = rs

uniMap = IM.fromList $ [
 f '→' LReservedOp "->",
 f '←' LReservedOp "<-",
 f '∷' LReservedOp "::",
 f '‥' LReservedOp "..",
 f '⇒' LReservedOp "=>"
 ] where
 f c w v = (ord c, (w,v))

splitName :: String -> (Bool,String,String)
splitName s = ('.' `elem` rmod, reverse rmod, reverse rid) where
    (rid,'.':rmod) = span ('.' /=) (reverse s)

fromL (L _ _ x) = x
pragma s ((fromL -> "{-#"):(fromL -> n):ls) | map toUpper n == s = d [] ls  where
    d cs (L _ _ "#-}":rs) = (Just $ reverse cs,rs)
    d cs (r:rs) = d (r:cs) rs
    d cs [] = (Just (reverse cs),[])
pragma "LINE" ((L bsl LPragmaStart "#LINE"):ls) = d [] ls  where
    d cs (r@(L sl _ _):rs) | srcLocLine sl /= srcLocLine bsl = (Just $ reverse cs,r:rs)
    d cs (r:rs) = d (r:cs) rs
    d cs [] = (Just (reverse cs),[])
pragma _ ls = (Nothing,ls)

skipComments :: [Lexeme] -> [Lexeme]
skipComments ls = f ls where
    f (L bsl _ "#LINE":rs) = f (d rs) where
        d (r@(L sl _ _):rs) | srcLocLine sl /= srcLocLine bsl = r:rs
        d (_:rs) = d rs
        d [] = []
    f (L sl _ "{-#":rs) = f (d sl rs) where
        d _ (L _ _ "#-}":rs) = rs
        d _ (L sl _ _:rs) = d sl rs
        d sl [] = [L sl LLexError "unterminated pragma"]
    f (l:ls) = l:f ls
    f [] = []

layoutStarters   = ["where","let","of","do", "\\case"]
