{-
        Copyright:        Mark Jones and The Hatchet Team
                          (see file Contributors)
        Module:           Representation
        Primary Authors:  Mark Jones and Bernie Pope
        Description:      The basic data types for representing objects
                          in the type inference algorithm.
        Notes:            See the file License for license information
                          Large parts of this module were derived from
                          the work of Mark Jones' "Typing Haskell in
                          Haskell", (http://www.cse.ogi.edu/~mpj/thih/)
-}

module Representation(
    Type(..),
    Tyvar(..),
    tyvar,
    Tycon(..),
    fn,
    Kind(..),
    Kindvar(..),
    unfoldKind,
    Pred(..),
    Qual(..),
    findType,
    Class,
    Subst,
    getTypeCons,
    flattenType,
    Scheme(..),
    FlattenType(..),
    Assump(..),
    tList
    )where


import Control.Monad.Trans
import Data.Generics
import Data.IORef
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ(nest,Doc)

import Atom
import Binary
import Doc.DocLike
import Doc.PPrint(pprint,PPrint)
import HsSyn
import Name.Name
import Name.Names
import Name.VConsts
import Options
import qualified Doc.DocLike as D
import qualified FlagDump as FD
import Utils


--------------------------------------------------------------------------------

-- Types


data Type  = TVar {-# UNPACK #-} !Tyvar
           | TCon !Tycon
           | TAp  Type Type
           | TGen {-# UNPACK #-} !Int {-# UNPACK #-} !Tyvar
           | TArrow Type Type
             deriving(Data,Typeable, Ord, Show)
    {-! derive: GhcBinary !-}

instance TypeNames Type where
    tBool = TCon (Tycon (nameName tc_Bool) Star)
    tString = TAp tList tChar
    tChar      = TCon (Tycon (nameName tc_Char) Star)
    tUnit = TCon (Tycon (nameName tc_Unit) Star)

tList = TCon (Tycon (nameName tc_List) (Kfun Star Star))

instance Eq Type where
    (TVar a) == (TVar b) = a == b
    (TCon a) == (TCon b) = a == b
    (TAp a' a) == (TAp b' b) = a' == b' && b == a
    (TGen a _) == (TGen b _) = a == b
    (TArrow a' a) == (TArrow b' b) = a' == b' && b == a
    _ == _ = False


-- Unquantified type variables

data Tyvar = Tyvar { tyvarAtom :: {-# UNPACK #-} !Atom, tyvarName ::  !HsName, tyvarKind :: Kind, tyvarRef :: Maybe (IORef (Maybe Type)) }
    deriving(Data,Typeable)
    {-  derive: GhcBinary -}

instance Show Tyvar where
    showsPrec _ Tyvar { tyvarName = hn, tyvarKind = k, tyvarRef = Just _ } = shows hn . (":-" ++) . shows k
    showsPrec _ Tyvar { tyvarName = hn, tyvarKind = k } = shows hn . ("::" ++) . shows k

findType :: MonadIO m => Type -> m Type
findType tv@(TVar Tyvar {tyvarRef = Just r }) = liftIO $ do
    rt <- readIORef r
    case rt of
        Nothing -> return tv
        Just t -> do
            t' <- findType t
            writeIORef r (Just t')
            return t'
findType tv = return tv

refType (TVar tv@Tyvar {tyvarRef = Nothing}) = do
    r <- newIORef Nothing
    return $ TVar tv { tyvarRef = Just r }
refType t = return t

unrefType (TVar tv) =  TVar tv { tyvarRef = Nothing }
unrefType t = t


class FlattenType t where
    flattenType' ::  t -> IO t

flattenType :: (FlattenType t, MonadIO m) => t -> m t
flattenType t = liftIO (flattenType' t)

instance FlattenType t => FlattenType [t] where
   flattenType' xs = mapM flattenType' xs

instance FlattenType Pred where
    flattenType' (IsIn c t) = do
        t' <- flattenType' t
        return $ IsIn c t'

instance FlattenType t => FlattenType (Qual t) where
    flattenType' (ps :=> t) = do
        ps' <- flattenType' ps
        t' <- flattenType' t
        return $ ps' :=> t'

instance FlattenType Type where
    flattenType' tv =  do
        tv' <- findType tv
        let ft (TAp x y) = do
                x' <- flattenType' x
                y' <- flattenType' y
                return $ TAp x' y'
            ft (TArrow x y) = do
                x' <- flattenType' x
                y' <- flattenType' y
                return $ TArrow x' y'
            ft t = return t
        ft tv'

instance FlattenType Scheme where
    flattenType' (Forall ks qt) = flattenType' qt >>= return . Forall ks

instance FlattenType Assump where
    flattenType' (ks :>: qt) = flattenType' qt >>= return . (:>:) ks

instance FlattenType t => FlattenType (Map.Map x t) where
    flattenType' mp = sequence [flattenType' y >>= return . (,) x| (x,y) <- Map.toAscList mp] >>= return . Map.fromDistinctAscList

instance Show (IORef a) where
    showsPrec _ _ = ("<IORef>" ++)

tyvar n k = Tyvar (fromString $ fromHsName n) n k

instance Eq Tyvar where
    Tyvar { tyvarAtom = x } == Tyvar { tyvarAtom = y } = x == y
    Tyvar { tyvarAtom = x } /= Tyvar { tyvarAtom = y } = x /= y

instance Ord Tyvar where
    compare (Tyvar { tyvarAtom = x }) (Tyvar { tyvarAtom = y }) = compare x y
    (Tyvar { tyvarAtom = x }) <= (Tyvar { tyvarAtom = y }) = x <= y
    (Tyvar { tyvarAtom = x }) >= (Tyvar { tyvarAtom = y }) = x >= y
    (Tyvar { tyvarAtom = x }) <  (Tyvar { tyvarAtom = y })  = x < y
    (Tyvar { tyvarAtom = x }) >  (Tyvar { tyvarAtom = y })  = x > y



-- Type constructors

data Tycon = Tycon HsName Kind
    deriving(Data,Typeable, Eq, Show,Ord)
    {-! derive: GhcBinary !-}

instance ToTuple Tycon where
    toTuple n = Tycon (toTuple n) (foldr Kfun Star $ replicate n Star)
instance ToTuple Type where
    toTuple n = TCon $ toTuple n
instance ToTuple Scheme where
    toTuple n = Forall [] ([] :=> toTuple n)

-- pretty printing for types etc:

instance PPrint Doc Type where
  pprint t = fst $ runVarName [] Utils.nameSupply $ prettyPrintTypeM t

-- the trickery is to map TVars and TGens into nice
-- variable names: a, b, c, d, and so on when we print them

prettyPrintTypeM :: Type -> VarName Doc
prettyPrintTypeM t
   = case t of
           TVar (Tyvar { tyvarName = tv }) -> do
                            findResult <- lookupInMap t
                            case findResult of
                               Nothing -> do nm <- nextName
                                             updateVMap (t, nm)
                                             return (text nm)
                               --Just v  -> return $ text v
                               Just v  -> return $ text v <> tyvar (text (show tv))
           TCon tycon -> return $ pprint tycon
           -- check for the Prelude.[] special case
           TAp t1 t2  -> do case tList == t1 of
                               True  -> do doc  <- prettyPrintTypeM t2
                                           return $ brackets doc
                               False -> do doc1 <- prettyPrintTypeM t1
                                           doc2 <- maybeParensAp t2
                                           return $ doc1 <+> doc2
           TGen _ (Tyvar { tyvarName = tv })   -> do
                            findResult <- lookupInMap t
                            case findResult of
                                Nothing -> do
                                    nm <- nextName
                                    updateVMap (t, nm)
                                    return (text nm)
                                    --return (text nm <> parens (text (show tv)))
                                Just v  -> return $ text v <> tyvar (text (show tv))
           TArrow t1 t2 -> do doc1 <- maybeParensArrow t1
                              doc2 <- prettyPrintTypeM t2
                              return $ doc1 <> text " -> " <> doc2
    where
    -- puts parentheses around the doc for a type if needed
    maybeParensAp :: Type -> VarName Doc
    maybeParensAp t
       = do case t of
               TAp t1 _   -> do case tList == t1 of
                                   True  -> prettyPrintTypeM t
                                   False -> do doc <- prettyPrintTypeM t
                                               return $ parens doc
               _anything  -> maybeParensArrow t
    maybeParensArrow :: Type -> VarName Doc
    maybeParensArrow t
       = do case t of
               TArrow {} -> do doc <- prettyPrintTypeM t
                               return $ parens doc
               _  -> prettyPrintTypeM t
    tyvar d = if dump FD.Tyvar then parens d else empty


instance PPrint Doc Tycon where
   pprint (Tycon i _) = pprint i

infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TArrow a b

--------------------------------------------------------------------------------

-- Kinds

data Kind  = Star
           | Kfun Kind Kind
           | KVar Kindvar               -- variables aren't really allowed in haskell in kinds
             deriving(Data,Typeable, Eq, Ord, Show)   -- but we need them for kind inference
    {-! derive: GhcBinary !-}

newtype Kindvar = Kindvar Int deriving(Data, Binary,Typeable, Ord, Eq, Show)

instance DocLike d => PPrint d Kind where
   pprint Star = text "*"
   pprint (Kfun Star Star) = text "* -> *"
   pprint (Kfun k1   Star) = text "(" <> pprint k1 <> text ")" <> text " -> *"
   pprint (Kfun Star k2)   = text "* -> " <> pprint k2
   pprint (Kfun k1   k2)   = text "(" <> pprint k1 <> text ") -> (" <> pprint k2 <> text ")"
   pprint (KVar kindVar)   = pprint kindVar

instance DocLike d =>  PPrint d Kindvar where
   pprint (Kindvar s) = text $ 'k':show s

--  * -> * == [*,*]
--  (*->*->*) -> * -> * == [(*->*->*), *, *]
unfoldKind :: Kind -> [Kind]
unfoldKind Star = [Star]
unfoldKind (KVar v) = [KVar v]
unfoldKind (Kfun k1 k2) = k1 : unfoldKind k2

--------------------------------------------------------------------------------

-- Predicates
data Pred   = IsIn Class Type
              deriving(Data,Typeable, Show, Eq,Ord)
    {-! derive: GhcBinary !-}

instance PPrint Doc Pred where
  -- pprint (IsIn c t) = pprint c <+> pprint t
  pprint pred
     = fst $ runVarName [] Utils.nameSupply $ prettyPrintPredM pred

prettyPrintPredM :: Pred -> VarName Doc
prettyPrintPredM (IsIn c t)
   = do typeDoc <- prettyPrintTypeM t
        return $ pprint c <+> typeDoc

-- Qualified entities
data Qual t =  [Pred] :=> t
              deriving(Data,Typeable, Show, Eq,Ord)
    {-! derive: GhcBinary !-}

prettyPrintQualPredM :: Qual Pred -> VarName Doc
prettyPrintQualPredM (preds :=> pred)
   = do case preds of
           []            -> prettyPrintPredM pred
           [p]           -> do leftPredDoc  <- prettyPrintPredM p
                               rightPredDoc <- prettyPrintPredM pred
                               return $ hsep [leftPredDoc, text "=>", rightPredDoc]
           preds@(_:_:_) -> do docs <- mapM prettyPrintPredM preds
                               let predsDoc = parens (hcat (punctuate comma docs))
                               rightPredDoc <- prettyPrintPredM pred
                               return $ hsep [predsDoc, text "=>", rightPredDoc]



-- special case for qualified types
prettyPrintQualTypeM :: Qual Type -> VarName Doc
prettyPrintQualTypeM (preds :=> t)
   = do case preds of
           []            -> prettyPrintTypeM t
           [p]           -> do predDoc <- prettyPrintPredM p
                               typeDoc <- prettyPrintTypeM t
                               return $ hsep [predDoc, text "=>", typeDoc]
           preds@(_:_:_) -> do docs <- mapM prettyPrintPredM preds
                               let predsDoc = parens (hcat (punctuate comma docs))
                               typeDoc <- prettyPrintTypeM t
                               return $ hsep [predsDoc, text "=>", typeDoc]

-- Class
type Class = HsName

--instance PPrint Doc t => PPrint Doc (Qual t) where
--  pprint (ps :=> t) = pptuple ps <+> text "=>" <+> pprint t

instance PPrint Doc (Qual Pred) where
    pprint inst = fst $ runVarName [] Utils.nameSupply $ prettyPrintQualPredM inst

instance PPrint Doc (Qual Type) where
    pprint inst = fst $ runVarName [] Utils.nameSupply $ prettyPrintQualTypeM inst

--prettyPrintInst :: Inst -> Doc
--prettyPrintInst inst
--   = fst $ runVarName [] Utils.nameSupply $ prettyPrintQualPredM inst

--------------------------------------------------------------------------------

-- substitutions

type Subst = Map.Map Tyvar Type

--------------------------------------------------------------------------------

getTypeCons (TCon (Tycon n _)) = n
getTypeCons (TAp a _) = getTypeCons a
getTypeCons (TArrow {}) = nameName tc_Arrow
getTypeCons x = error $ "getTypeCons: " ++ show x

-- schemes


data Scheme = Forall [Kind] (Qual Type)
              deriving(Data,Typeable, Eq, Show, Ord)
    {-! derive: GhcBinary !-}

instance PPrint Doc Scheme where
  pprint scheme
    = fst $ runVarName [] Utils.nameSupply $ prettyPrintSchemeM scheme

prettyPrintSchemeM :: Scheme -> VarName Doc
prettyPrintSchemeM (Forall _kinds qType)
   = prettyPrintQualTypeM qType

--------------------------------------------------------------------------------

-- assumptions

data Assump =  (:>:) HsName Scheme
    deriving(Ord,Eq,Data,Typeable, Show)
    {-! derive: GhcBinary !-}

instance  PPrint Doc Assump where
  pprint (i :>: s) = (text (show i) <+> text ":>:") <$> nest 2 (pprint s)


--------------------------------------------------------------------------------

-- a monad for matching type variables with nice names for pretty printing

newtype VarName a = VarName (State -> (a, State))  deriving(Typeable)

type VMap = [(Type, String)]  -- maps type (vars) to strings
type NameSupply = [String]    -- a fresh name supply

data State = State {
      vmap  :: VMap,       -- the map of variables to names
      names :: NameSupply  -- a fresh name Supply
   } deriving(Typeable)

instance Monad VarName where
    return a
        = VarName (\state -> (a, state))
    VarName comp >>= fun
        = VarName (\state -> let (result, newState) = comp state
                                 VarName comp' = fun result
                             in comp' newState)

runVarName :: VMap -> NameSupply -> VarName a -> (a, State)
runVarName varMap nameSupp (VarName comp)
   = (result, newState)
   where
   (result,newState)
      = comp (State {vmap  = varMap,
                     names = nameSupp})

select :: (State -> a) -> VarName a
select selector = VarName (\state -> (selector state, state))

getVMap :: VarName VMap
getVMap = select vmap

updateVMap :: (Type, String) -> VarName ()
updateVMap newEntry
   = VarName (\state -> let oldmap = vmap state
                        in ((), state {vmap = newEntry : oldmap}))

nextName :: VarName String
nextName
   = VarName (\state -> let oldNames = names state
                        in (head oldNames, state {names = tail oldNames}))

lookupInMap :: Type -> VarName (Maybe String)
lookupInMap t = do
    m <- getVMap
    return $ lookup t m


instance Binary Tyvar where
    put_ bh (Tyvar aa ab ac ad) = do
            put_ bh aa
            put_ bh ab
            put_ bh ac
            --put_ bh ad
    get bh = do
    aa <- get bh
    ab <- get bh
    ac <- get bh
    --ad <- get bh
    --ad <- newIORef Nothing
    return (Tyvar aa ab ac Nothing)

