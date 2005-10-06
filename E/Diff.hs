
-- | Attempt to find pretty printable differences between terms.


module E.Diff where

import E.E
import E.Inline
import FreeVars
import Stats

-- | take two expressions and return (hopefully smaller) expressions with their differences

diff ::  E -> E -> (E,E)
diff a b = f a b where
    f (ELetRec ds e) (ELetRec ds' e') = (ELetRec (g ds ds') e, ELetRec (g ds' ds) e')
    f a b = (a,b)
    g ds ds' = [ d | d@(v,e) <- ds, not (lookup v ds' == Just e)  ]


-- show terms which contain interesting free variables
findOddFreeVars  :: [TVr] -> E -> E
findOddFreeVars fs (ELetRec ds e) = ELetRec [ ds | ds@(_,e) <- ds, any (`elem` fs) (freeVars e) ] e
findOddFreeVars _ e = e


printEStats :: E -> IO ()
printEStats e = do
    stats <- Stats.new
    let f e@ELam {} = tick stats "lambda" >> emapE' f e
        f e@EVar {} = tick stats "var-use" >> return e
        f e@(ELetRec ds _) = ticks stats (length ds) "let-binding" >> emapE' f e
        f e@EPi {} = tick stats "pi" >> emapE' f e
        f e@ELit {} = tick stats "lit" >> emapE' f e
        f e@EPrim {} = tick stats "prim" >> emapE' f e
        f e@EError {} = tick stats "error" >> emapE' f e
        f e@ECase {} = do
            tick stats "case"
            ticks stats (length $ caseBodies e) "case-alt"
            emapE' f e
        f e = tick stats "other" >> emapE' f e
    f e
    Stats.print "E" stats




