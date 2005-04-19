module Grin.Parse where

import Text.ParserCombinators.Parsec.Language(haskellStyle)
import Text.ParserCombinators.Parsec.Token
import Atom
import Char

tp = haskellStyle {
   reservedOpNames = ["=","<-","->"],
   reservedNames = ["return","fetch","store","update","case", "end"]
   }   


exp = 

val = try unit <|> parens tp node  <|>  liti <|> litc <|> var where
    unit = symbol "()" >> return Unit 
    liti = do x <- integer tp; return $ Lit (fromIntegral x) tInt  
    litc = do x <- charLiteral tp; return $ Lit (chr x) tChar
    tag = do
        t@(c:_) <- identifier tp
        if isUpper c then return $ Tag (toAtom t) else return (Var (atomIndex $ toAtom t) TyUnit)
    node = do
        n <- cov
        



