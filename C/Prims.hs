module C.Prims where

import Data.Monoid
import Data.Generics
import Binary
import Doc.DocLike
import Doc.PPrint

data Requires = Requires {
    reqIncludes :: [String],
    reqLibraries :: [String]
    } deriving(Typeable, Data, Eq, Ord, Show)
    {-! derive: Monoid, GhcBinary !-}

type ExtType = String

emptyExtType = ""

data Prim =
    PrimPrim String          -- Special primitive implemented in the compiler somehow.
    | CConst String ExtType  -- C code which evaluates to a constant
    | Operator String  [ExtType] ExtType   -- C operator
    | Func Bool String [ExtType] ExtType   -- function call with C calling convention
    | IFunc [ExtType] ExtType              -- indirect function call
    | AddrOf String                        -- address of linker name
    | Peek ExtType                         -- read value from memory
    | Poke ExtType                         -- write value to memory
    | CCast ExtType ExtType                -- Cast from one basic type to another, possibly lossy.
    deriving(Typeable, Data, Eq, Ord, Show)
    {-! derive: GhcBinary !-}

-- | These primitives may safely be duplicated without affecting performance or
-- correctness too adversly. either because they are cheap to begin with, or
-- will be recombined in a later pass.

primIsCheap :: Prim -> Bool
primIsCheap AddrOf {} = True
primIsCheap CCast {} = True
primIsCheap CConst {} = True
primIsCheap Operator {} = True
primIsCheap _ = False

aprimIsCheap (APrim p _) = primIsCheap p

parsePrimString s = do
    ws@(_:_) <- return $ words s
    let v = case last ws of
            '&':s -> AddrOf s
            s -> Func False s [] emptyExtType
    let f opt@('-':'l':_) = Requires [] [opt]
        f s = Requires [s] []
    return (APrim v (mconcat (map f (init ws))))


primPrim s = APrim (PrimPrim s) mempty

data APrim = APrim Prim Requires
    deriving(Typeable, Data, Eq, Ord, Show)
    {-! derive: GhcBinary !-}

instance PPrint d Prim  => PPrint d APrim where
    pprint (APrim p _) = pprint p

instance DocLike d => PPrint d Prim where
    pprint (PrimPrim t) = text t
    pprint (CConst s t) = parens (text t) <> parens (text s)
    pprint (Operator s xs r) = parens (text r) <> text s <> tupled (map text xs)
    pprint (Func _ s xs r) = parens (text r) <> text s <> tupled (map text xs)
    pprint (IFunc xs r) = parens (text r) <> parens (char '*') <> tupled (map text xs)
    pprint (AddrOf s) = char '&' <> text s
    pprint (Peek t) = char '*' <> text t
    pprint (Poke t) = char '=' <> text t
    pprint (CCast _ t) = parens (text t)

