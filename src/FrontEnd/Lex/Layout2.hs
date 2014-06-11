module FrontEnd.Lex.Layout2 where

import FrontEnd.Lex.Lexer
import FrontEnd.SrcLoc
import FrontEnd.Lex.Layout
import qualified Data.Map as Map

data Context
    = LO String Loc Int
    | NL String

data R = R
    {is :: [Token Lexeme]
    ,os :: [Lexeme]
    ,ctx :: [Context]
    }

data Loc
    = InHead
    | InGuard
    | InRhs
    | InRhsGuard
    | InCaseGuard
    | InCaseHead
    | InCaseRhsGuard
    | InCaseRhs
    | InDo
    | InIgnore
    deriving(Eq,Ord)

data Action
    = NewLoc Loc
    | RBrace

actionList :: [([Loc],[String],Action)]
actionList =
    [([InHead],["type","data","instance","deriving"],NewLoc InIgnore)
    ,([InHead],["::"],NewLoc InRhs)

    ,([InHead],["="],NewLoc InRhs)
    ,([InHead],["|"],NewLoc InGuard)
    ,([InGuard],["="],NewLoc InRhsGuard)
    ,([InRhsGuard],["|"],NewLoc InGuard)

    ,([InCaseHead],["->"],NewLoc InCaseRhs)
    ,([InCaseHead],["|"],NewLoc InCaseGuard)
    ,([InCaseGuard],["->"],NewLoc InCaseRhsGuard)
    ,([InCaseRhsGuard],["|"],NewLoc InCaseGuard)

    ,([InRhsGuard,InRhs,InIgnore],[";"],NewLoc InHead)
    ,([InCaseRhsGuard,InCaseRhs],[";"],NewLoc InCaseHead)

    ,([InRhs,InCaseRhs],["|"],RBrace)
    ,([InRhs],[","],RBrace)
  --  ,([InRhs,InCaseRhs,InCaseRhsGuard,InRhsGuard],["="],RBrace)
 --   ,([InCaseRhs],["="],RBrace)
    ]

actionMap = Map.fromList [ ((x,y),a) | (x,y,a) <- actionList, x <- x, y <- y]

layout :: [Token Lexeme] -> [Lexeme]
layout ls = f R { is = ls, ctx = [], os = [] } where
    f R { .. }
        | LO _  InDo _:ctx <- ctx, (Token (L _ _ "where"):_) <- is = f R { os = rbrace:os, .. }
        | LO wlo loc n:cs <- ctx, TokenNL m:is <- is = case compare m n of
            -- a semicolon is invalid before a 'where' so close it
            EQ | Token (L _ _ "where"):_ <- is ->  f R { ctx = cs, is = TokenNL m:is, os = rbrace:os, .. }
            -- normal cases
            EQ -> f R { is = Token semi:is, .. }
            LT -> f R { ctx = cs, is = TokenNL m:is, os = rbrace:os, .. }
            GT -> f R { .. }
        | TokenNL m:is <- is = f R { .. }

        | TokenVLCurly (L _ _ s) n:is <- is, LO "do" _ l:_ <- ctx, n >= l
            = f R { ctx = LO s (sl s) n:ctx, os = lbrace:os, .. }
        | TokenVLCurly (L _ _ s) n:is <- is, LO _ _ l:_ <- ctx, n > l
            = f R { ctx = LO s (sl s) n:ctx, os = lbrace:os, .. }
        | TokenVLCurly (L _ _ s) n:is <- is, LO _ _ l:_ <- ctx, n == l
            = f R { os = rbrace:lbrace:os, .. }
        | TokenVLCurly (L _ _ s) n:is <- is
            = f R { ctx = LO s (sl s) n:ctx, os = lbrace:os, .. }

        | Token m@(L _ _ "then"):is <- is, NL "then":ctx <- ctx
            = f R { os = m:os, ctx = NL "else":ctx, .. }
        | Token m@(L _ _ ms):is <- is, NL close:ctx <- ctx, ms == close = f R { os = m:os, .. }
        | Token m@(L _ _ ms):is <- is, Just close <- lookup ms layoutBrackets = f R { os = m:os, ctx = NL close:ctx, .. }

        | Token m@(L _ _ ms):is' <- is, Just opener <- lookup ms closingBrackets = case ctx of
            (NL opened:ctx) | opener == opened -> f R { os = m:os, is = is',..}
                            | otherwise -> f R { os = err ("found "++  ms++  " but expected " ++ opened):m:os, is = is',.. }
            LO {}:ctx -> f R { os = rbrace:os, .. }

        | Token m@(L _ _ ident):is <- is, LO open loc n:ctx' <- ctx,
            Just action <- Map.lookup (loc,ident) actionMap = case action of
                RBrace ->  f R { os = m:rbrace:os, ctx = ctx',.. }
                (NewLoc loc') -> f R { os = m:os, ctx = LO open loc' n:ctx',.. }
        | Token (L sl nt "let#"):is' <- is = f R { is = is', os = (L sl nt "let":os), ..}

        | Token m:ms <- is = f R  { is = ms, os = m:os, .. }
        | [] <- is, NL s:ctx <- ctx = f R { os = err ("expected " ++ s):os, .. }
        | [] <- is, LO {}:ctx <- ctx = f R { os = rbrace:os, .. }
        | [] <- is = reverse os
        where
        (sloc:_) = [ sl | L sl _ _ <- os] ++ [bogusASrcLoc]
        semi = L sloc LSpecial ";"
        rbrace = L sloc LSpecial "}"
        lbrace = L sloc LSpecial "{"
        err = L sloc LLexError
        nl :: Char -> Int -> [Lexeme]
        nl _ _ = []
        nl t m = [L sloc LLexError ("\n" ++ replicate m ' ')]
        nl t m = [L sloc LLexError ("\t(" ++ [t] ++ ")\n" ++ replicate m ' ')]

        sl "do" = InDo
        sl "of" = InCaseHead
        sl _ = InHead
        -- sl s = f s where
        --     f "do" = LocDo
        --     f "of" = bl { locCase = True }
        --     f "\\case" = bl { locCase = True }
        --     f _ = bl
        --     bl = Loc { locRhs = False, locGuard = False, locCase = False }

layoutBrackets =
    [("case","of")
    ,("if","then")
    ,("(",")")
    ,("let","in")
    ,("(#","#)")
    ,("[","]")
    ,("{","}")]

closingBrackets = [ (y,x) | (x,y) <- layoutBrackets]-- ++
     --[("in","let")]
