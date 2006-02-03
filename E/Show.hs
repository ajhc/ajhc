module E.Show(ePretty,render,prettyE,ePrettyEx) where

import Char
import Control.Monad.Identity
import qualified Data.Set as Set

import Atom
import Doc.Attr
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.FreeVars()
import Support.FreeVars
import Name.Name
import Name.Names
import Name.VConsts
import Options
import qualified Doc.Chars as UC
import qualified FlagDump as FD
import Support.Unparse
import Util.VarName

render :: Doc -> String
render doc =  displayS (renderPretty 0.95 (optColumns options)  doc) ""

prettyE :: E -> String
prettyE e = render $ ePretty e

ePrettyEx = ePretty

showId :: DocLike d => Id -> d
showId 0 = (char '_')
showId i | Just x <- intToAtom i  = (text $ show  $ (fromAtom x :: Name))
showId i = (text $ 'x':show i)

instance DocLike d => PPrint d TVr where
    pprint TVr { tvrIdent = i }  = showId i

instance PPrint Doc E where
    pprint x = ePretty x


newtype SEM a = SEM { unSEM :: VarNameT E Id String Identity a }
    deriving(Monad,Functor)


showLit ::
    (a -> SEM (Unparse Doc))   -- ^ routine for showing the contents of constructor literals
    -> Lit a E                 -- ^ the literal to show
    -> SEM (Unparse Doc)       -- ^ the final result
showLit showBind l = do
    let const_color = col "blue"
    let f (LitInt c t) | t == tCharzh, i >= 0x20 && i < 0x7f = return $ atom $ (const_color (tshow $ chr i)) where
            i = fromIntegral c
        f (LitInt i _) = return $ atom $ (const_color (text $ show i))
        f (LitCons n [] t) | t == tTag = return $  atom $ (const_color (text $ show n))
        f (LitCons s es _) | Just n <- fromTupname s , n == length es = do
            es' <- mapM (fmap unparse . showBind) es
            return $ atom $ tupled es'
        f (LitCons s es _) | Just n <- fromUnboxedNameTuple s, n == length es = do
            es' <- mapM (fmap unparse . showBind) es
            return $ atom $ encloseSep (text "(# ") (text " #)") (text ", ") es'
        f (LitCons n [a,b] _) | dc_Cons == n  = do
            a' <- showBind a
            b' <- showBind b
            return $ a' `cons` b'
        f (LitCons n [e] _) | tc_List == n = do
            e <- showBind e
            return $  atom   (char '[' <> unparse e  <> char ']')
        f (LitCons n [] _) | dc_EmptyList == n = return $ atom $ text "[]"
        f (LitCons s es t) = do
            es' <- mapM showBind es
            return $ foldl app (atom (tshow s)) es' -- `inhabit` prettye t
        cons = bop (R,5) (text ":")
    f l

app = bop (L,100) (text " ")
col n x = attrColor (attr oob) n x
attr = if dump FD.Html then html else ansi

showI i = do
    n <- SEM $ maybeLookupName i
    case n of
        Nothing -> showId i
        Just n -> text n


showTVr :: TVr -> SEM (Unparse Doc)
showTVr TVr { tvrIdent = i, tvrType =  t, tvrInfo = nfo}  = do
    let si = if dump FD.EInfo then (<> tshow nfo) else id
    ty <- showE t
    ii <- showI i
    return $ atom (si ii) `inhabit` ty
showTVr' TVr { tvrIdent = i} = do
    ii <- showI i
    return $ atom ii


allocTVr :: TVr -> SEM a -> SEM a
allocTVr tvr action | tvrIdent tvr == 0 = action
allocTVr tvr action | tvrType tvr == eStar  = do
    SEM $ newName (map (:[]) ['a' ..]) eStar (tvrIdent tvr)
    action
allocTVr tvr action | tvrType tvr == eStar `tFunc` eStar  = do
    SEM $ newName (map (('f':) . show) [0::Int ..])  (tvrType tvr) (tvrIdent tvr)
    action
allocTVr tvr action | even (tvrIdent tvr) = do
    SEM $ newName (map (('v':) . show) [1::Int ..]) Unknown (tvrIdent tvr)
    action
allocTVr _ action = action


showE :: E -> SEM (Unparse Doc)
showE e = do
    let const_color = col "blue"
    let f e | Just s <- E.E.toString e = return $ atom $ const_color (text $ show s)
        f e | Just xs <- toList e = do
            xs <- mapM (fmap unparse . showE) xs
            return $ atom $ list xs
        f (EAp a b) = liftM2 app (showE a) (showE b)
        f (EPi (TVr { tvrIdent = 0, tvrType =  e1}) e2) = liftM2 arr (showE e1) (showE e2)
        f (EPi (TVr { tvrIdent = n, tvrType =  e1}) e2) | not $ n `Set.member` freeVars e2 = liftM2 arr (showE e1) (showE e2)
        f (EPi tvr@(TVr {  tvrType =  z}) e) | z == eStar = allocTVr tvr $ do
            tvr <- showTVr' tvr
            liftM2 dot (return $ pop (retOp UC.forall) tvr) (showE e)
        f (EPi t e) = allocTVr t $ do
            tvr <- showTVr t
            e <- showE e
            return $ (pop (retOp UC.pI) tvr) `dot` e
        f (ELam tvr@TVr {tvrType =  z} e) | z == eStar = allocTVr tvr $ do
            tvr <- showTVr' tvr
            liftM2 dot (return $ pop (retOp UC.lAmbda) tvr) (showE e)
        f (ELam t e) = allocTVr t $ do
            tvr <- showTVr t
            e <- showE e
            return $ (pop (retOp UC.lambda) tvr) `dot` e
        f (EVar tvr) = showTVr' tvr
        f Unknown = return $ symbol (char  '?')
        f (ESort EStar) = return $ symbol UC.star
        f (ESort EBox) = return $ symbol UC.box
        f (ESort EHash) = return $ symbol (text "#")
        f (ELit l) = showLit showE l
        f (EError s t) = do
            ty <- showE t
            return $ atom $ angles ( UC.bottom <> char ':' <> text s <>  UC.coloncolon <> unparse ty)
        f (EPrim s es t) = do
            es' <- mapM showE es
            t <- showE t
            return $ atom $ angles $ unparse $ foldl app (atom (pprint s)) es' `inhabit` t
        f (ELetRec ds e) = do
            e <- fmap unparse $ showE e
            ds <- mapM (fmap unparse . showDecl) ds
            return $ fixitize (L,(-10)) $ atom $ group (nest 4  ( keyword "let" </> (align $ sep (map (<> bc ';') ds) </> (keyword "in" <+> e))))

        f ec@(ECase { eCaseScrutinee = e, eCaseAlts = alts }) = allocTVr (eCaseBind ec) $ do
            scrut <- fmap unparse $ showE e
            alts <- mapM showAlt alts
            dcase <- case (eCaseDefault ec) of
                Nothing -> return []
                Just e -> do
                    let ecb = eCaseBind ec

                    db <- showTVr (if tvrIdent ecb `Set.member` freeVars e then ecb else ecb { tvrIdent = 0 })
                    e <- showE e
                    return [unparse db <+> UC.rArrow <+> unparse e]
            let alts' = map (<> bc ';') (alts ++ dcase)
            return $ fixitize ((L,(-10))) $ atom $
                group ( nest 4 ( keyword "case" <+> scrut <+> keyword "of" <$>  (align $ sep (alts'))) )
        showAlt (Alt l e) = foldr allocTVr ans (litBinds l) where
            ans = do
                l <- showLit showTVr l
                e <- showE e
                return $ nest 4 $ fill 10 ((unparse l) <+>  UC.rArrow </> (unparse e))
        showDecl (t,e) = do
            t <- subSEM $ showTVr t
            e <- subSEM $ showE e
            return $ atom $ nest 4 $ unparse t <+> retOp (char '=') </> unparse e
        bold' = bold
        bc = bold' . char
        keyword x = col "magenta" (text x)
        symbol x = atom (bold' x)
        arr = bop (R,0) $ retOp (space <> UC.rArrow <> space)
        dot = bop (R,-1) $ retOp (char '.')

    f e

subSEM (SEM act) = SEM $ subVarName act


retOp x = col "lightgreen" x
inhabit = bop (N,-2) $ retOp UC.coloncolon
bold :: Doc -> Doc
bold = attrBold (attr oob)

ePretty e = unparse pe where
    (SEM pe') = showE e
    Identity pe = runVarNameT pe'

tTag = rawType "tag#"
rawType s  = ELit (LitCons (toName RawType s) [] eHash)

