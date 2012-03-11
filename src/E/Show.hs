module E.Show(ePretty,render,prettyE) where

import Control.Monad.Identity
import Data.Char(chr)
import Data.Maybe

import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.FreeVars()
import E.TypeCheck
import Name.Id
import Name.Name
import Name.Names
import Name.VConsts
import Options
import Support.FreeVars
import Support.Unparse
import Util.SetLike
import Util.VarName
import qualified Data.Map as Map
import qualified Doc.Chars as UC
import qualified FlagDump as FD

{-# NOINLINE render #-}
{-# NOINLINE ePretty #-}
{-# NOINLINE prettyE #-}
render :: Doc -> String
render doc =  displayS (renderPretty 100.0 (optColumns options)  doc) ""

prettyE :: E -> String
prettyE e = render $ ePretty e

instance DocLike d => PPrint d TVr where
    pprint TVr { tvrIdent = i }  = pprint i

instance PPrint Doc E where
    pprint x = ePretty x

instance PPrint String E where
    pprintAssoc a i x = render $ pprintAssoc a i x

instance PPrint String (Lit E E) where
    pprintAssoc _ n x | n <= 9    = prettyE (ELit x)
                      | otherwise = parens (prettyE (ELit x))

newtype SEM a = SEM { _unSEM :: VarNameT E Id String Identity a }
    deriving(Monad,Functor)

enumList = [
    (tc_Bool_,["False#","True#"]),
    (toName TypeConstructor ("Jhc.Order","Ordering#"),["LT#","EQ#","GT#"])
    ]

showLit ::
    (a -> SEM (Unparse Doc))   -- ^ routine for showing the contents of constructor literals
    -> Lit a E                 -- ^ the literal to show
    -> SEM (Unparse Doc)       -- ^ the final result
showLit showBind l = do
    let f (LitInt i (ELit LitCons { litName = n })) | Just l <- lookup n enumList, i >= 0 && fromIntegral i < length l =
            return $ atom $ ((text $ l !! (fromIntegral i)))
        f (LitInt n (ELit LitCons { litName = t})) | t == tc_Char_ = return $ atom $ tshow (chr $ fromIntegral n) <> char '#'
        f (LitInt i t) | dump FD.EVerbose = do
            se <- showE t
            return $ (atom (text $ show i) `inhabit` se )
        f (LitInt i _) = return $ atom $ ((text $ show i))
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
--        f LitCons { litName = n, litArgs = [] } | Just m <- getModule n, m `elem`[toModule "Jhc.Prim.Bits", toModule "Jhc.Prim.Word"]  = return $ atom $ text "[]"
--        f LitCons { litName = ((tc_Addr_ ==) -> True), litType = ((eHash ==) -> True) } = return $ atom $ text "Addr_"
--        f LitCons { litName = ((tc_FunAddr_ ==) -> True), litType = ((eHash ==) -> True) } = return $ atom $ text "FunAddr_"
--        f LitCons { litName = ((tc_Char_ ==) -> True), litType = ((eHash ==) -> True) } = return $ atom $ text "Char_"
--        f LitCons { litName = n, litArgs = [v] }
--        f LitCons { litName = n, litArgs = [v] }
--            | n == dc_Integer = go "Integer#"
--            | n == dc_Int     = go "Int#"
--            | n == dc_Char    = go "Char#"
--          where go n = do
--                    se <- showBind v
--                    return $ atom (text n) `app` se
        f LitCons { litName = s, litArgs = es, litType = t,
		    litAliasFor = Just af } | dump FD.EAlias = do
	    s <- return $ fromMaybe s (shortenName s)
            es' <- mapM showBind es
            se <- showE af
            return $ foldl appCon (atom (tshow s <> char '@' <> parens (unparse se))) es' -- `inhabit` prettye t
        f LitCons { litName = s, litArgs = es, litType = t } = do
	    s <- return $ fromMaybe s (shortenName s)
            es' <- mapM showBind es
            return $ foldl appCon (atom (tshow s)) es' -- `inhabit` prettye t
        cons = bop (R,5) (text ":")
	shortenName n = Map.lookup n shortName `mplus` (getModule n >>= mm) where
	    mm m = if m `elem` shortMods then return (toUnqualified n) else Nothing
            shortMods = map toModule [ "Jhc.Prim.IO", "Jhc.Prim.Bits", "Jhc.Type.Word", "Jhc.Type.C" ]
    f l

app = bop (L,100) (text " ")
appCon = bop (L,99) (text " ")

showI i = do
    n <- SEM $ maybeLookupName i
    case n of
        Nothing -> pprint i
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
allocTVr tvr action | tvrIdent tvr == emptyId = action
allocTVr tvr (SEM action) | tvrType tvr == eStar  = do
    SEM $ subVarName $ newName (map (:[]) ['a' ..]) eStar (tvrIdent tvr) >> action
allocTVr tvr (SEM action) | tvrType tvr == eStar `tFunc` eStar  = do
    SEM $ subVarName $ newName (map (('f':) . show) [0::Int ..])  (tvrType tvr) (tvrIdent tvr) >> action
allocTVr tvr (SEM action) | not $ isJust (fromId (tvrIdent tvr)) = do
    SEM $ subVarName $ newName (map (('v':) . show) [1::Int ..]) Unknown (tvrIdent tvr) >> action
allocTVr _ action = action

-- collects lambda and pi abstractions
collectAbstractions e0 = go e0 [] where
    go e1@(EPi tvr e)  xs | tvrIdent tvr == emptyId          = done e1 xs
                          | not (sortKindLike (tvrType tvr)) = go e ((UC.pI,     tvr, True) :xs)
                          | tvrType tvr /= eStar             = go e ((UC.forall, tvr, True) :xs)
                          | dump FD.EVerbose || tvrIdent tvr `member` (freeVars e::IdSet)
                                                             = go e ((UC.forall, tvr, False):xs)
                          | otherwise                        = done e1 xs
    go e1@(ELam tvr e) xs | tvrType tvr == eStar             = go e ((UC.lAmbda, tvr, False):xs)
                          | sortKindLike (tvrType tvr)       = go e ((UC.lAmbda, tvr, True) :xs)
                          | otherwise                        = go e ((UC.lambda, tvr, True) :xs)
    go  e           xs = done e xs
    done e xs = (reverse xs, e)

short_names = [
      tc_Bool,      tc_Char,     tc_IO,      tc_ACIO,    tc_State_,
      tc_RealWorld, tc_Ordering, tc_Bool_,   tc_Ratio,   tc_Float,
      tc_Double,    tc_Ptr,      tc_FunPtr,  tc_Integer, tc_Addr_,
      tc_FunAddr_,  tc_Char_,    dc_Boolzh,  dc_Char,    dc_Integer,
      tc_ST,        tc_Bang_]

shortName = Map.fromList [ (x, toUnqualified x) | x <- short_names]

showE :: E -> SEM (Unparse Doc)
showE e = do
    let f e | Just s <- E.E.toString e = return $ atom $ (text $ show s)
        f e | Just xs <- eToList e = do
            xs <- mapM (fmap unparse . showE) xs
            return $ atom $ list xs
        f e | e == tRational = return $ atom $ text "Rational"
        f e | e == tString   = return $ atom $ text "String"
        f e | e == tUnit     = return $ atom $ text "()"
        f e | e == tWorld__  = return $ atom $ text "RealWorld_"
        f e | e == vUnit     = return $ atom $ text "()"
        f (EAp a b) = liftM2 app (showE a) (showE b)
        f (EPi (TVr { tvrIdent = eid, tvrType =  e1}) e2) | eid == emptyId = liftM2 arr (showE e1) (showE e2)
        f (EPi (TVr { tvrIdent = n, tvrType =  e1}) e2) | not $ dump FD.EVerbose, not $ n `member` (freeVars e2 ::IdSet) = liftM2 arr (showE e1) (showE e2)
        f e0 | (as@(_:_), e) <- collectAbstractions e0 =
            foldr (\(_, tvr, _) -> allocTVr tvr)
                  (do tops <- mapM p as
                      e <- showE e
                      return (fixitize (N,1) $ atom $ group $ (align $ skipToNest <> fillCat tops) <$> unparse e))
                  as
            where
              p :: (Doc, TVr, Bool) -> SEM Doc
              p (c,t,detailed) = do tvr <- if detailed then showTVr t else showTVr' t
                                    return (c <> unparse tvr <> (char '.'))
        f (EVar tvr) = if dump FD.EVerbose then showTVr tvr else showTVr' tvr
        f Unknown = return $ symbol (char  '?')
        f (ESort s) = return $ symbol (tshow s)
        f (ELit (LitCons { litName = n, litArgs = [ELit (LitInt i _)] })) | n == dc_Char = return $ atom $ tshow $ chr (fromIntegral i)
        f (ELit l) = showLit showE l
        f (EError "" t) = do
            ty <- showE t
            return $ atom $ angles (text "exitFailure"  <>  UC.coloncolon <> unparse ty)
        f (EError s t) = do
            ty <- showE t
            return $ atom $ angles ( UC.bottom <> char ':' <> text s <>  UC.coloncolon <> unparse ty)
        f (EPrim s es t) = do
            es' <- mapM showE es
            t <- showE t
            return $ atom $ angles $ unparse $ foldl app (atom (pprint s)) es' `inhabit` t
        f ELetRec { eDefs = ds, eBody = e } = foldr (\(tvr,_) -> allocTVr tvr) (do
            e <- fmap unparse $ showE e
            ds <- mapM (fmap unparse . showDecl) ds
            return $ fixitize (N,98) $ atom $ nest 2 (group ( keyword "let"
                                                                  <$> (align $ sep (map (<> char ';') ds))
                                                                  <$> (keyword "in")) </> e )) ds

        f ec@(ECase { eCaseScrutinee = e, eCaseAlts = alts }) = mt (showE (eCaseType ec)) $  allocTVr (eCaseBind ec) $ do
            scrut <- fmap unparse $ showE e
            alts <- mapM showAlt alts
            let ecb = eCaseBind ec
                isUsed = tvrIdent ecb `member` (freeVars (caseBodies ec) :: IdSet)
            db <- showTVr (if dump FD.EVerbose || isUsed then ecb else ecb { tvrIdent = emptyId })
            dcase <- case (eCaseDefault ec) of
                Nothing -> return []
                Just e -> do
                    e <- showE e
                    return [unparse db <+> UC.rArrow </> unparse e]
            let alts' = map (\a -> nest 2 (group (a <> char ';'))) (alts ++ dcase)
            let mbind | isJust (eCaseDefault ec) = empty
                      | (isUsed && isNothing (eCaseDefault ec)) || dump FD.EVerbose = text " " <> (if isUsed then id else (char '_' <>)) (unparse db) <+> text "<-"
                      | otherwise = empty
            return $ fixitize ((N,98)) $ atom $
                group (nest 2 ( keyword "case" <> mbind <+> scrut <+> keyword "of" <$>  (align $ vcat alts')) )
        f _ = error "undefined value in E.Show"
        showAlt (Alt l e) = foldr allocTVr ans (litBinds l) where
            ans = do
                l <- showLit showTVr l
                e <- showE e
                return $ unparse l <+> UC.rArrow </> unparse e
        showDecl (t,e) = do
            t <- subSEM $ showTVr t
            e <- subSEM $ showE e
            return $ atom $ nest 2 $ group $ unparse t <+> (char '=') </> unparse e
        keyword x = text x
        symbol x = atom x
        arr = bop (R,0) $ (space <> UC.rArrow <> space)
        mt t x | dump FD.EVerbose = do
                    t <- t
                    x <- x
                    return $ x `inhabit` t
        mt _ x = x

    f e

subSEM (SEM act) = SEM $ subVarName act
inhabit = bop (N,-2) $ UC.coloncolon

ePretty e = unparse pe where
    (SEM pe') = showE e
    Identity pe = runVarNameT pe'

-- skip to the current nesting level, breaking the line if already past it
skipToNest      = column (\k ->
                  nesting (\i -> if k > i
                                 then linebreak
                                 else text (replicate (i-k) ' ')))
