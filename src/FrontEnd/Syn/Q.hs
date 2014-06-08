module FrontEnd.Syn.Q where

import Control.Monad.Writer hiding(lift)
import FrontEnd.HsSyn
import FrontEnd.Warning
import Name.Names
import Util.Std
import Util.UniqueMonad
import qualified Util.Seq as Seq

newtype Q a = Q { unQ :: UniqT (Writer (Seq.Seq Warning)) a }
    deriving(Monad,Applicative,Functor,UniqueProducer, MonadWarn)

runQ :: MonadWarn m => Q a -> m a
runQ (Q x) = do
    let (a,w) = runWriter (execUniqT 1 x)
    mapM_ addWarning (Seq.toList w)
    return a

class FromName a where
    newFromNameS :: Name -> a  -- set name parameters as appropriate for the type
    newFromName  :: Name -> a  -- use name exactly

newVarN :: FromName a => String -> Maybe Module -> Q (Name,a)
newVarN s m = do
    u <- newUniq
    let n = mkComplexName emptyNameParts {
        nameModule = m,
        nameIdent = s ++ show u,
        nameUniquifier = Nothing
        }
    return $! n `seq` (n,newFromNameS n)

newVar :: FromName a => Maybe Module -> Q (Name,a)
newVar = newVarN "q"

instance FromName HsExp where
    newFromNameS n = newFromName (nameTyLevel_s termLevel n)
    newFromName n
        | isConstructor n = HsCon (nameTyLevel_s termLevel n)
        | otherwise  = HsVar (nameTyLevel_s termLevel n)

instance FromName HsType where
    newFromNameS n = newFromName (nameTyLevel_s typeLevel n)
    newFromName n
        | isConstructor n = HsTyCon (nameTyLevel_s typeLevel n)
        | otherwise  = HsTyVar (nameTyLevel_s termLevel n)

instance FromName HsPat where
    newFromNameS n = newFromName (nameTyLevel_s typeLevel n)
    newFromName n
        | isConstructor n = HsPApp (nameTyLevel_s typeLevel n) []
        | otherwise  = HsPVar (nameTyLevel_s termLevel n)

type Exp = HsExp

class Lift t where
  lift :: t -> Q Exp

instance Lift Integer where
  lift x = return (HsLit (HsInt (fromInteger x)))

instance Lift Int where
  lift x = return (HsLit (HsInt (fromIntegral x)))

instance Lift Char where
  lift x = return (HsLit (HsChar x))

instance Lift Bool where
  lift True  = return (HsCon dc_True)
  lift False = return (HsCon dc_False)

instance Lift () where
  lift ()  = return (HsCon dc_Unit)

instance Lift a => Lift (Maybe a) where
  lift Nothing  = return (HsCon dc_Nothing)
  lift (Just x) = HsApp (HsCon dc_Just) <$>  lift x
{-

instance (Lift a, Lift b) => Lift (Either a b) where
  lift (Left x)  = liftM (ConE leftName  `AppE`) (lift x)
  lift (Right y) = liftM (ConE rightName `AppE`) (lift y)

instance Lift a => Lift [a] where
  lift xs = do { xs' <- mapM lift xs; return (ListE xs') }

liftString :: String -> Q Exp
-- Used in TcExpr to short-circuit the lifting for strings
liftString s = return (LitE (StringL s))

instance (Lift a, Lift b) => Lift (a, b) where
  lift (a, b)
    = liftM HsTup $ sequence [lift a, lift b]

instance (Lift a, Lift b, Lift c) => Lift (a, b, c) where
  lift (a, b, c)
    = liftM TupE $ sequence [lift a, lift b, lift c]

instance (Lift a, Lift b, Lift c, Lift d) => Lift (a, b, c, d) where
  lift (a, b, c, d)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d]

instance (Lift a, Lift b, Lift c, Lift d, Lift e)
      => Lift (a, b, c, d, e) where
  lift (a, b, c, d, e)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e]

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f)
      => Lift (a, b, c, d, e, f) where
  lift (a, b, c, d, e, f)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e, lift f]

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g)
      => Lift (a, b, c, d, e, f, g) where
  lift (a, b, c, d, e, f, g)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e, lift f, lift g]
-}
