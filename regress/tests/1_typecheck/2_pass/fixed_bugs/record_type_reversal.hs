module Module where
newtype WrappedArrow a b c = WrapArrow { unwrapArrow :: a b c }

foo :: WrappedArrow Either Bool Int
foo = WrapArrow (Left True)

