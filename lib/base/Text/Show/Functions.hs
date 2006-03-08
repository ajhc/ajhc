module Text.Show.Functions where


instance Show (a -> b) where
    showsPrec _ _ = showString "<function>"
