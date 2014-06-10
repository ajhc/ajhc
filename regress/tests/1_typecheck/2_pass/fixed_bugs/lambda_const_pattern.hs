module Module where

-- this hit a bug in lambda desugaring

type Series a = Int -> [a]

class Serial a where
  series   :: Series a
  -- | A proper 'coseries' implementation should pass the depth unchanged to
  -- its first argument. Doing otherwise will make enumeration of curried
  -- functions non-uniform in their arguments.
  coseries :: Series b -> Series (a->b)

-- Thanks to Ralf Hinze for the definition of coseries
-- using the nest auxiliary.
instance (Serial a, Serial b) => Serial (a->b) where
  series = coseries series
  coseries rs d =
    [ \ f -> g [ f a | a <- args ]
    | g <- nest args d ]
    where
    args = series d
    nest []     _ = [ \[] -> c | c <- rs d ]
    nest (a:as) _ = [ \(b:bs) -> f b bs | f <- coseries (nest as) d ]
