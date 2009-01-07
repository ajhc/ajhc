class Arrow a where
    arr :: (b -> c) -> a b c
    (>>>) :: a b c -> a c d -> a b d

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Arrow (Kleisli m) where
    arr f = Kleisli (return . f)
    Kleisli f >>> Kleisli g = Kleisli (\x -> f x >>= g)

main :: IO ()
main = return ()
