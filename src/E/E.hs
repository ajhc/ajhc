{-# OPTIONS -fglasgow-exts #-}
module E.E(
    Id(),
    IdMap(),
    IdSet(),
    newIds,
    module E.Type,
    module E.E,
    module E.FreeVars
    ) where

import Char(chr)
import Maybe
import qualified Data.Traversable as T

import Control.Monad.Identity
import C.Prims
import E.FreeVars
import E.Type
import Name.Id
import Name.Name
import Name.Names
import Name.VConsts
import Util.Gen

isWHNF ELit {} = True
isWHNF ELam {} = True
isWHNF EPi {} = True
isWHNF ESort {} = True
isWHNF ELetRec { eBody = e } = isWHNF e
isWHNF _ = False

-----------
-- E values
-----------

instance TypeNames E where
    tStar = eStar
    tInt = ELit (litCons { litName = tInt, litArgs = [], litType = eStar })
    tRational = ELit (litCons { litName = tc_Ratio, litArgs = [tInteger], litType = eStar })
    tChar = ELit (litCons { litName = tChar, litArgs = [], litType = eStar })
    tBool = ELit (litCons { litName = tBool, litArgs = [], litType = eStar })
    tUnit = ELit (litCons { litName = tUnit, litArgs = [], litType = eStar })
    tString =  (ELit (litCons { litName = tc_List, litArgs = [tChar], litType = eStar }))
    tInteger = ELit (litCons { litName = tInteger, litArgs = [], litType = eStar })
    tWorld__ = ELit (litCons { litName = tWorld__, litArgs = [], litType = eHash })
    tIntzh = ELit (litCons { litName = tIntzh, litArgs = [], litType = eHash })
    tEnumzh = ELit (litCons { litName = tEnumzh, litArgs = [], litType = eHash })
    tIntegerzh = ELit (litCons { litName = tIntegerzh, litArgs = [], litType = eHash })
    tCharzh = ELit (litCons { litName = tCharzh, litArgs = [], litType = eHash })

instance ConNames E where
    vTrue = ELit vTrue
    vFalse = ELit vFalse
    vUnit  = ELit vUnit

instance ConNames (Lit E E) where
    vTrue  = (litCons { litName = dc_Boolzh, litArgs = [ELit (LitInt 1 tEnumzh)], litType = tBool })
    vFalse = (litCons { litName = dc_Boolzh, litArgs = [ELit (LitInt 0 tEnumzh)], litType = tBool })
    vUnit  = (litCons { litName = dc_Unit, litArgs = [], litType = tUnit })

-- values
tFunc a b = ePi (tVr emptyId a) b
tvrSilly = tVr sillyId Unknown

-----------------
-- E constructors
-----------------

ePi a b = EPi a b

eLam v (EError s t) = EError s (ePi v t)
eLam v t = ELam v t

-- | throw away first n EPi terms
discardArgs :: Int -> E -> E
discardArgs 0 e = e
discardArgs n (EPi _ b) | n > 0 = discardArgs (n - 1) b
discardArgs _ _ = error "discardArgs"

tvrName :: Monad m => TVr  -> m Name
tvrName (TVr {tvrIdent =  n }) | Just a <- fromId n = return a
tvrName tvr = fail $ "TVr is not Name: " ++ show tvr

tvrShowName :: TVr -> String
tvrShowName t = maybe ('x':(show $ tvrIdent t)) show (tvrName t)

modAbsurd = "Jhc@.Absurd"
modBox    = "Jhc@.Box"

nameConjured :: String -> E -> Name
nameConjured mod n = toName TypeConstructor (mod,f n "") where
    f (ESort s) = shows s
    f (EPi TVr { tvrType = t1 } t2) = ('^':) . f t1 . f t2
    f _ = error $ "nameConjured: " ++ show (mod,n)

fromConjured :: Monad m => String -> Name -> m E
fromConjured mod n = maybeM ("fromConjured: " ++ show (mod,n)) $ do
    let f s = funit s `mplus` flam s
        flam ('^':xs) = do (x,rs) <- f xs; (y,gs) <- f rs; return (EPi tvr { tvrType = x } y,gs)
        flam _ = Nothing
        funit ('*':xs) = return (eStar,xs)
        funit ('#':xs) = return (eHash,xs)
        funit ('!':xs) = return (ESort EBang,xs)
        funit ('(':'#':')':xs) = return (ESort ETuple,xs)
        funit _ = Nothing
    (TypeConstructor,(mod',an)) <- return $  fromName n
    guard (mod' == mod)
    (r,"") <- f an
    return r

isBottom EError {} = True
isBottom _ = False

caseBodiesMapM :: Monad m => (E -> m E) -> E -> m E
caseBodiesMapM f ec@ECase { eCaseAlts = as, eCaseDefault = d } = do
    let g (Alt l e) = f e >>= return . Alt l
    as' <- mapM g as
    d' <- T.mapM f d
    return $ caseUpdate ec { eCaseAlts = as', eCaseDefault = d' }
caseBodiesMapM _ _ = error "caseBodiesMapM"

eToList :: Monad m => E -> m  [E]
eToList (ELit LitCons { litName = n, litArgs = [e,b] }) | dc_Cons == n = eToList b >>= \x -> return (e:x)
eToList (ELit LitCons { litName = n, litArgs = [] }) | dc_EmptyList == n = return []
eToList _ = fail "eToList: not list"

toString (ELit LitCons { litName = n, litArgs = [], litType = t }) = if dc_EmptyList == n && t == tString then return "" else fail "not a string"
toString x = eToList x >>= mapM fromChar where
    fromChar (ELit LitCons { litName = dc, litArgs = [ELit (LitInt ch t)] }) | dc == dc_Char && t == tCharzh = return (chr $ fromIntegral ch)
    fromChar _ = fail "fromChar: not char"

ltTuple ts = ELit $ litCons { litName = nameTuple TypeConstructor (length ts), litArgs = ts, litType = eStar }
ltTuple' ts = ELit $ litCons { litName = unboxedNameTuple TypeConstructor (length ts), litArgs = ts, litType = eHash }

p_unsafeCoerce = primPrim "unsafeCoerce"
p_dependingOn = primPrim "dependingOn"
p_toTag = primPrim "toTag"
p_fromTag = primPrim "fromTag"

fromUnboxedTuple :: Monad m => E -> m [E]
fromUnboxedTuple (ELit LitCons { litName = n, litArgs = as }) | Just _ <- fromUnboxedNameTuple n = return as
fromUnboxedTuple _ = fail "fromUnboxedTuple: not a tuple"

isUnboxedTuple m = isJust (fromUnboxedTuple m)

instance Show E where
    showsPrec d (EAp aa ab) = showParen (d >= 10)
	      (showString "EAp" . showChar ' ' . showsPrec 10 aa . showChar ' ' .
	       showsPrec 10 ab)
    showsPrec d (ELam aa ab) = showParen (d >= 10)
	      (showString "ELam" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (EPi aa ab) | tvrIdent aa == emptyId = showParen (d >= 10)
	      (showsPrec 10 (tvrType aa) . showString " -> " .
	       showsPrec 10 ab)
    showsPrec d (EPi aa ab) = showParen (d >= 10)
	      (showString "EPi" . showChar ' ' . showsPrec 10 aa . showChar ' ' .
	       showsPrec 10 ab)
    showsPrec d (EVar aa) = showParen (d >= 10)
	      (showString "EVar" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Unknown) = showString "Unknown"
    showsPrec d (ESort aa) = showsPrec d aa
    --showsPrec d (ESort aa) = showParen (d >= 10)
    --          (showString "ESort" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (ELit aa) = showsPrec 10 aa
    --showsPrec d (ELit aa) = showParen (d >= 10)
    --          (showString "ELit" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (ELetRec aa ab) = showParen (d >= 10)
	      (showString "ELetRec" . showChar '{' .
	       showString "eDefs" . showChar '=' . showsPrec 10 aa
	       . showChar ',' .
	       showString "eBody" . showChar '=' . showsPrec 10 ab
	       . showChar '}')
    showsPrec d (EPrim aa ab ac) = showParen (d >= 10)
	      (showString "EPrim" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)
    showsPrec d (EError aa ab) = showParen (d >= 10)
	      (showString "EError" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (ECase aa ab ac ad ae af) = showParen (d >= 10)
	      (showString "ECase" . showChar '{' .
	       showString "eCaseScrutinee" . showChar '=' . showsPrec 10 aa
	       . showChar ',' .
	       showString "eCaseType" . showChar '=' . showsPrec 10 ab
	       . showChar ',' .
	       showString "eCaseBind" . showChar '=' . showsPrec 10 ac
	       . showChar ',' .
	       showString "eCaseAlts" . showChar '=' . showsPrec 10 ad
	       . showChar ',' .
	       showString "eCaseDefault" . showChar '=' . showsPrec 10 ae
	       . showChar ',' .
	       showString "eCaseAllFV" . showChar '=' . showsPrec 10 af
	       . showChar '}')

instance Show e => Show (Alt e) where
    showsPrec n (Alt l e) = showParen (n > 10) $ shows l . showString " -> " . shows e
