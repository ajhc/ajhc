module FrontEnd.Lex.ParseMonad where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import FrontEnd.SrcLoc
import FrontEnd.Warning
import Options
import qualified Util.Seq as Seq

-- monad/applicative functor with warning, failure and environment. unlinke
-- standard writting monads, this also collects warnings from failures.
--
-- in order to do warning collection we should prefer the applicative forms of
-- composure rather than the monad ones

data PEnv = PEnv { envSrcSpan :: SrcSpan, envOptions :: Opt }
type PWritten = (Seq.Seq Warning)

--newtype P a = P { unP :: ReaderT PEnv (WriterT PWritten Maybe) a }
--    deriving(Monad,MonadReader PEnv, MonadWriter PWritten,Applicative,Functor)
newtype P a = P { unP :: PEnv -> (PWritten,Maybe a) }
--    deriving(Monad,MonadReader PEnv, MonadWriter PWritten,Applicative,Functor)

instance Functor P where
    fmap f (P m) = P $ \e -> fmap (fmap f) (m e)
instance Applicative P where
    pure x = P $ \_ -> (mempty, Just x)
    P fa *> P fb = P $ \e -> fa e `g` fb e where
        (a,b) `g` (x,y) = (a `mappend` x,b *> y)
    P fa <*> P fb = P $ \e -> case (fa e,fb e) of
        ((w,Just f),(w',Just a)) -> (w `mappend` w',Just (f a))
        ((w,_),(w',_)) -> (w `mappend` w',Nothing)

instance Monad P where
    (>>) = (*>)
    return = pure
    P fa >>= fb = P $ \e -> case fa e of
        (w,Nothing) -> (w,Nothing)
        (w,Just v) -> case unP (fb v) e of
            (w',r) -> (w `mappend` w',r)
    fail s = P $ \ PEnv { .. } -> (Seq.singleton (Warning (srcLoc envSrcSpan) WarnFailure s),Nothing)

parseNothing = P $ \_ -> (mempty,Nothing)

instance MonadReader PEnv P where
    ask = P (\e -> (mempty, Just e))
    local f (P fn) = P (fn . f)
instance MonadWriter PWritten P where
    tell xs = P (\_ -> (xs, Just ()))
    listen (P fn) = P $ \e -> case fn e of (w,Just a) -> (w,Just (a,w))
                                           (w,Nothing) -> (w,Nothing)
    pass (P fn) = P $ \e -> case fn e of
        (w,Nothing) -> (w,Nothing)
        (w,Just (a,wf)) -> (wf w,Just a)
instance OptionMonad P where
    getOptions = asks envOptions

instance MonadSetSrcLoc P where
    withSrcSpan' ss = local (\e -> e { envSrcSpan = ss })

instance MonadSrcLoc P where
    getSrcSpan = asks envSrcSpan

instance MonadWarn P where
    addWarning w = tell (Seq.singleton w)

runP :: P a -> Opt -> ([Warning],Maybe a)
runP (P fn) opt = case fn PEnv { envOptions = opt, envSrcSpan = bogusSrcSpan } of
    (x,y) -> (Seq.toList x,y)
