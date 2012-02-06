module Control.Monad (
    Functor(fmap),  Monad((>>=), (>>), return, fail),  MonadPlus(mzero, mplus),
    mapM,  mapM_,  forM,  forM_,  sequence,  sequence_,  (=<<),  (>=>),  (<=<),
    forever,  void,  join,  msum,  filterM,  mapAndUnzipM,  zipWithM,
    zipWithM_,  foldM,  foldM_,  replicateM,  replicateM_,  guard,  when,
    unless,  liftM,  liftM2,  liftM3,  liftM4,  liftM5,  ap
    ) where

class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a
    mzero = fail "mzero"

instance MonadPlus Maybe where
    mzero = Nothing
    Nothing `mplus` y = y
    x `mplus` _ = x

instance MonadPlus [] where
    mzero = []
    mplus = (++)

-- Functions

msum  :: MonadPlus m => [m a] -> m a
msum xs  =  foldr mplus mzero xs

join             :: (Monad m) => m (m a) -> m a
join x           =  x >>= id

when             :: (Monad m) => Bool -> m () -> m ()
when p s         =  if p then s else return ()

unless           :: (Monad m) => Bool -> m () -> m ()
unless p s       =  when (not p) s

ap               :: (Monad m) => m (a -> b) -> m a -> m b
ap               =  liftM2 ($)

guard            :: MonadPlus m => Bool -> m ()
guard p          =  if p then return () else mzero

mapAndUnzipM     :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs = sequence (map f xs) >>= return . unzip

zipWithM         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys =  sequence (zipWith f xs ys)

zipWithM_         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys =  sequence_ (zipWith f xs ys)

foldM            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM f a []     =  return a
foldM f a (x:xs) =  f a x >>= \ y -> foldM f y xs

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p []     = return []
filterM p (x:xs) = do { b  <- p x;
ys <- filterM p xs;
return (if b then (x:ys) else ys)
   }

liftM            :: (Monad m) => (a -> b) -> (m a -> m b)
liftM f          =  \a -> do { a' <- a; return (f a') }

liftM2           :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
liftM2 f         =  \a b -> do { a' <- a; b' <- b; return (f a' b') }

liftM3           :: (Monad m) => (a -> b -> c -> d) ->
                                 (m a -> m b -> m c -> m d)
liftM3 f         =  \a b c -> do { a' <- a; b' <- b; c' <- c;
   return (f a' b' c') }

liftM4           :: (Monad m) => (a -> b -> c -> d -> e) ->
                                 (m a -> m b -> m c -> m d -> m e)
liftM4 f         =  \a b c d -> do { a' <- a; b' <- b; c' <- c; d' <- d;
     return (f a' b' c' d') }

liftM5           :: (Monad m) => (a -> b -> c -> d -> e -> f) ->
                                 (m a -> m b -> m c -> m d -> m e -> m f)
liftM5 f         =  \a b c d e -> do { a' <- a; b' <- b; c' <- c; d' <- d;
       e' <- e; return (f a' b' c' d' e') }

-- extensions

-- | Like 'foldM', but discards the result.
foldM_            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
foldM_ f a xs     = foldM f a xs >> return ()

-- | @'replicateM' n act@ performs the action @n@ times,
-- gathering the results.
replicateM        :: (Monad m) => Int -> m a -> m [a]
replicateM n x    = sequence (replicate n x)

-- | Like 'replicateM', but discards the result.
replicateM_       :: (Monad m) => Int -> m a -> m ()
replicateM_ n x   = sequence_ (replicate n x)

-- | 'forM' is 'mapM' with its arguments flipped
forM            :: Monad m => [a] -> (a -> m b) -> m [b]
{-# INLINE forM #-}
forM            = flip mapM

-- | 'forM_' is 'mapM_' with its arguments flipped
forM_           :: Monad m => [a] -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_           = flip mapM_

infixr 1 <=<, >=>

-- | Left-to-right Kleisli composition of monads.
(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

-- | Right-to-left Kleisli composition of monads. '(>=>)', with the
-- arguments flipped
(<=<)       :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<)       = flip (>=>)

-- | @'forever' act@ repeats the action infinitely.
forever     :: (Monad m) => m a -> m ()
forever a   = a >> forever a

void :: Monad m => m a -> m ()
void x = x >> return ()
