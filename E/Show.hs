module E.Show(ePretty,render,prettyE,ePrettyEx) where

import Char
import Control.Monad.Identity
import Maybe

import C.Prims
import Doc.Attr
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.FreeVars()
import Name.Id
import Name.Name
import Name.Names
import Name.VConsts
import Options
import Support.FreeVars
import Support.Unparse
import Util.SetLike
import Util.VarName
import qualified Doc.Chars as UC
import qualified FlagDump as FD

render :: Doc -> String
render doc =  displayS (renderPretty 0.95 (optColumns options)  doc) ""

prettyE :: E -> String
prettyE e = render $ ePretty e

ePrettyEx = ePretty

showId :: DocLike d => Id -> d
showId 0 = (char '_')
showId i | Just x <- fromId i  = (text $ show x)
showId i = (text $ 'x':show i)

instance DocLike d => PPrint d TVr where
    pprint TVr { tvrIdent = i }  = showId i

instance PPrint Doc E where
    pprint x = ePretty x

instance PPrint String E where
    pprint x = prettyE x

instance PPrint String (Lit E E) where
    pprintPrec n x | n <= 9    = prettyE (ELit x)
                   | otherwise = parens (prettyE (ELit x))

newtype SEM a = SEM { _unSEM :: VarNameT E Id String Identity a }
    deriving(Monad,Functor)

enumList = [
    (tc_Boolzh,["False#","True#"]),
    (toName TypeConstructor ("Jhc.Order","Ordering#"),["LT#","EQ#","GT#"])
    ]

showLit ::
    (a -> SEM (Unparse Doc))   -- ^ routine for showing the contents of constructor literals
    -> Lit a E                 -- ^ the literal to show
    -> SEM (Unparse Doc)       -- ^ the final result
showLit showBind l = do
    let const_color = col "blue"
    let --f (LitInt c t) | t == tCharzh = return $ atom $ (const_color (tshow $ chr i)) where
        --    i = fromIntegral c
        f (LitInt i (ELit LitCons { litName = n })) | Just l <- lookup n enumList, i >= 0 && fromIntegral i < length l =
            return $ atom $ (const_color (text $ l !! (fromIntegral i)))
        f (LitInt i _) = return $ atom $ (const_color (text $ show i))
        f LitCons { litName = n, litArgs = [], litType = t } | t == tTag = return $  atom $ (const_color (text $ show n))
        f LitCons { litName = s, litArgs = es } | Just n <- fromTupname s , n == length es = do
            es' <- mapM (fmap unparse . showBind) es
            return $ atom $ tupled es'
        f LitCons { litName = s, litArgs = es } | Just n <- fromUnboxedNameTuple s, n == length es = do
            es' <- mapM (fmap unparse . showBind) es
            return $ atom $ encloseSep (text "(# ") (text " #)") (text ", ") es'
        f LitCons { litName = n, litArgs = [a,b] } | dc_Cons == n  = do
            a' <- showBind a
            b' <- showBind b
            return $ a' `cons` b'
        f LitCons { litName = n, litArgs = [e] } | tc_List == n = do
            e <- showBind e
            return $  atom   (char '[' <> unparse e  <> char ']')
        f LitCons { litName = n, litArgs = [] } | dc_EmptyList == n = return $ atom $ text "[]"
        f LitCons { litName = n, litArgs = [v] }
            | n == dc_Integer = go "Integer#"
            | n == dc_Int     = go "Int#"
            | n == dc_Char    = go "Char#"
          where go n = do
                    se <- showBind v
                    return $ atom (text n) `app` se
        f LitCons { litName = s, litArgs = es, litType = t, litAliasFor = Just af } | dump FD.EAlias = do
            es' <- mapM showBind es
            se <- showE af
            return $ foldl appCon (atom (tshow s <> char '@' <> parens (unparse se))) es' -- `inhabit` prettye t
        f LitCons { litName = s, litArgs = es, litType = t } = do
            es' <- mapM showBind es
            return $ foldl appCon (atom (tshow s)) es' -- `inhabit` prettye t
        cons = bop (R,5) (text ":")
    f l

app = bop (L,100) (text " ")
appCon = bop (L,99) (text " ")
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
allocTVr _tvr action | dump FD.EVerbose = action
allocTVr tvr action | tvrIdent tvr == 0 = action
allocTVr tvr (SEM action) | tvrType tvr == eStar  = do
    SEM $ subVarName $ newName (map (:[]) ['a' ..]) eStar (tvrIdent tvr) >> action
allocTVr tvr (SEM action) | tvrType tvr == eStar `tFunc` eStar  = do
    SEM $ subVarName $ newName (map (('f':) . show) [0::Int ..])  (tvrType tvr) (tvrIdent tvr) >> action
allocTVr tvr (SEM action) | even (tvrIdent tvr) = do
    SEM $ subVarName $ newName (map (('v':) . show) [1::Int ..]) Unknown (tvrIdent tvr) >> action
allocTVr _ action = action

tBoolzh = ELit litCons { litName = tc_Boolzh, litType = eHash, litAliasFor = Just tIntzh }

showE :: E -> SEM (Unparse Doc)
showE e = do
    let const_color = col "blue"
    let f e | Just s <- E.E.toString e = return $ atom $ const_color (text $ show s)
        f e | Just xs <- eToList e = do
            xs <- mapM (fmap unparse . showE) xs
            return $ atom $ list xs
        f e | e == tBool     = return $ atom $ text "Bool"
        f e | e == tBoolzh   = return $ atom $ text "Bool#"
        f e | e == tChar     = return $ atom $ text "Char"
        f e | e == tInt      = return $ atom $ text "Int"
        f e | e == tInteger  = return $ atom $ text "Integer"
        f e | e == tRational = return $ atom $ text "Rational"
        f e | e == tString   = return $ atom $ text "String"
        f e | e == tUnit     = return $ atom $ text "()"
        --f e | e == tWorld__  = return $ atom $ text "World__"
        f e | e == vFalse    = return $ atom $ text "False"
        f e | e == vTrue     = return $ atom $ text "True"
        f e | e == vUnit     = return $ atom $ text "()"
        f (EAp a b) = liftM2 app (showE a) (showE b)
        f (EPi (TVr { tvrIdent = 0, tvrType =  e1}) e2) = liftM2 arr (showE e1) (showE e2)
        f (EPi (TVr { tvrIdent = n, tvrType =  e1}) e2) | not $ dump FD.EVerbose, not $ n `member` (freeVars e2 ::IdSet) = liftM2 arr (showE e1) (showE e2)
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
        f (EVar tvr) = if dump FD.EVerbose then showTVr tvr else showTVr' tvr
        f Unknown = return $ symbol (char  '?')
        f (ESort s) = return $ symbol (tshow s)
        f (ELit l) = showLit showE l
        f (EError "" t) = do
            ty <- showE t
            return $ atom $ angles (text "exitFailure"  <>  UC.coloncolon <> unparse ty)
        f (EError s t) = do
            ty <- showE t
            return $ atom $ angles ( UC.bottom <> char ':' <> text s <>  UC.coloncolon <> unparse ty)
        f (EPrim (APrim Operator { primOp = op } _) [x,y] t) = do
            x <- showE x
            y <- showE y
            t <- showE t
            return $ atom $ angles $ unparse $ atom ((unparse x) <+> text op <+> (unparse y)) `inhabit` t
        f (EPrim (APrim CCast {} _) [x] t) = do
            x <- showE x
            t <- showE t
            return $ atom $ parens (unparse t) <> unparse x
        f (EPrim s es t) = do
            es' <- mapM showE es
            t <- showE t
            return $ atom $ angles $ unparse $ foldl app (atom (pprint s)) es' `inhabit` t
        f ELetRec { eDefs = ds, eBody = e } = do
            e <- fmap unparse $ showE e
            ds <- mapM (fmap unparse . showDecl) ds
            return $ fixitize (L,(-10)) $ atom $ group (nest 4  ( keyword "let" </> (align $ sep (map (<> bc ';') ds) </> (keyword "in" <+> e))))

        f ec@(ECase { eCaseScrutinee = e, eCaseAlts = alts }) = mt (showE (eCaseType ec)) $  allocTVr (eCaseBind ec) $ do
            scrut <- fmap unparse $ showE e
            alts <- mapM showAlt alts
            let ecb = eCaseBind ec
                isUsed = tvrIdent ecb `member` (freeVars (caseBodies ec) :: IdSet)
            db <- showTVr (if dump FD.EVerbose || isUsed then ecb else ecb { tvrIdent = 0 })
            dcase <- case (eCaseDefault ec) of
                Nothing -> return []
                Just e -> do
                    e <- showE e
                    return [unparse db <+> UC.rArrow <+> unparse e]
            let alts' = map (<> bc ';') (alts ++ dcase)
            let mbind | isJust (eCaseDefault ec) = empty
                      | (isUsed && isNothing (eCaseDefault ec)) || dump FD.EVerbose = text " " <> (if isUsed then id else (char '_' <>)) (unparse db) <+> text "<-"
                      | otherwise = empty
            return $ fixitize ((L,(-10))) $ atom $
                group ( nest 4 ( keyword "case" <> mbind <+> scrut <+> keyword "of" <$>  (align $ vcat (alts'))) )
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

        mt t x | dump FD.EVerbose = do
                    t <- t
                    x <- x
                    return $ x `inhabit` t
        mt _ x = x


    f e

subSEM (SEM act) = SEM $ subVarName act


retOp x = col "lightgreen" x
inhabit = bop (N,-2) $ retOp UC.coloncolon
bold :: Doc -> Doc
bold = attrBold (attr oob)

{-# NOINLINE ePretty #-}
ePretty e = unparse pe where
    (SEM pe') = showE e
    Identity pe = runVarNameT pe'

tTag = rawType "tag#"
rawType s  = ELit litCons { litName = toName RawType s, litType = eHash }

