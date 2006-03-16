-- | some useful types to use in Info's that don't really fit anywhere else
module Info.Types where

import Data.Dynamic
import Data.Monoid
import List
import qualified Data.Set as Set

import Atom
import Binary
import Info.Info as Info
import MapBinaryInstance()

-- | list of properties of a function, such as specified by use pragmas or options
newtype Properties = Properties (Set.Set Atom)
    deriving(Typeable,Eq,Binary,Monoid)

-- | how many manifest lambdas are in a functions definition
newtype Arity = Arity Int
    deriving(Typeable,Show,Ord,Eq,Num,Binary)

-- | how the variable is bound
data BindType = CaseDefault | CasePattern | LetBound | LambdaBound | PiBound
    deriving(Typeable,Show,Ord,Eq)

-- | whether the variable is exported from the current module
data ExportStatus = Exported
    deriving(Typeable,Show,Ord,Eq)

instance Binary ExportStatus where
    put_ _ _ = return ()
    get _ = return Exported


instance Show Properties where
    showsPrec _ (Properties s) = shows (sortBy (\x y -> compare (show x) (show y)) (Set.toList s))


-- These are set by user pragmas
prop_INLINE = toAtom "INLINE"
prop_SUPERINLINE = toAtom "SUPERINLINE"
prop_NOINLINE = toAtom "NOINLINE"
prop_SRCLOC_ANNOTATE = toAtom "SRCLOC_ANNOTATE"
prop_MULTISPECIALIZE = toAtom "MULTISPECIALIZE"

-- | this is set on functions which are the target of an error annotated function
prop_SRCLOC_ANNOTATE_FUN = toAtom "_SRCLOC_ANNOTATE_FUN"

-- | this is an internal flag set on instance functions
prop_INSTANCE = toAtom "_INSTANCE"

-- | this is an internal flag set on class methods to eventually be filled in
prop_METHOD = toAtom "_METHOD"

-- | whether a function is exported
prop_EXPORTED = toAtom "_EXPORTED"

prop_WORKER = toAtom "_WORKER"
prop_WRAPPER = toAtom "_WRAPPER"
prop_CYCLIC = toAtom "_CYCLIC"
prop_PLACEHOLDER = toAtom "_PLACEHOLDER"
prop_RULEBINDER = toAtom "_RULEBINDER"
prop_SCRUTINIZED = toAtom "_SCRUTINIZED"
prop_SPECIALIZATION = toAtom "_SPECIALIZATION"
prop_SUPERSPECIALIZE = toAtom "_SUPERSPECIALIZE"
prop_UNSHARED = toAtom "_UNSHARED"


class HasProperties a where
    setProperty :: Atom -> a -> a
    unsetProperty :: Atom -> a -> a
    getProperty :: Atom -> a -> Bool

instance HasProperties Properties where
    setProperty prop (Properties x) = Properties (Set.insert prop x)
    unsetProperty prop (Properties x) = Properties (Set.delete prop x)
    getProperty prop (Properties x) = Set.member prop x


instance HasProperties Info where
    setProperty prop info = case Info.lookup info of
        Just (Properties x) -> Info.insert (Properties $ Set.insert prop x) info
        Nothing -> Info.insert (Properties $ Set.singleton prop) info
    unsetProperty prop info = case Info.lookup info of
        Just pr@(Properties x) -> case Set.delete prop x of
                p | Set.null p -> Info.delete pr info
                  | otherwise -> Info.insert (Properties p) info
        Nothing -> info
    getProperty prop info = getProperty prop (Info.fetch info :: Properties)


setProperties :: HasProperties a => [Atom] -> a -> a
setProperties [] nfo = nfo
setProperties (p:ps) nfo = setProperty p (setProperties ps nfo)

