module FrontEnd.Tc.Type(
    module FrontEnd.Tc.Type,
    Type(..),
    Kind(..),
    findType,
    fn,
    Qual(..),
    Tyvar(..),
    Tycon(..),
    kind
    ) where

import Control.Monad.Trans
import Control.Monad.Writer
import Data.IORef

import Representation
import Type(kind)

type Box = IORef Type
type Sigma' = Sigma
type Tau' = Tau
type Rho' = Rho

type MetaTV = Tyvar
type SkolemTV = Tyvar
type BoundTV = Tyvar

openBox :: MonadIO m => Box -> m Sigma
openBox x = liftIO $ readIORef x

fillBox :: MonadIO m => Box -> Type -> m ()
fillBox x t | not (isBoxy t) = liftIO $ writeIORef x t
fillBox x t = error "attempt to fillBox with boxy type"

isTau :: Type -> Bool
isTau TForAll {} = False
isTau TBox {} = False
isTau (TAp a b) = isTau a && isTau b
isTau (TArrow a b) = isTau a && isTau b
isTau _ = True

isTau' :: Type -> Bool
isTau' TForAll {} = False
isTau' (TAp a b) = isTau a && isTau b
isTau' (TArrow a b) = isTau a && isTau b
isTau' _ = True

isBoxy :: Type -> Bool
isBoxy TBox {} = True
isBoxy (TForAll _ (_ :=> t)) = isBoxy t
isBoxy (TAp a b) = isBoxy a || isBoxy b
isBoxy (TArrow a b) = isBoxy a || isBoxy b
isBoxy _ = False

isRho' :: Type -> Bool
isRho' TForAll {} = False
isRho' _ = True

isRho :: Type -> Bool
isRho r = isRho' r && not (isBoxy r)


fromTAp t = f t [] where
    f (TAp a b) rs = f a (b:rs)
    f t rs = (t,reverse rs)
