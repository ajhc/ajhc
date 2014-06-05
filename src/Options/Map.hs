module Options.Map(processLanguageFlags,languageDefault,generateJhcParams) where

-- LANGUAGE -> FlagOpts mapping

import Data.Char(toLower)
import Data.List(sort)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified FlagOpts as FO

{-# NOINLINE processLanguageFlags #-}
processLanguageFlags :: [String] -> Set.Set FO.Flag -> (Set.Set FO.Flag,[String])
processLanguageFlags xs opt = f xs opt [] where
    f [] opt rs = (opt,reverse rs)
    f (('N':'o':ls):xs) opt rs | Just o <- readM ls = g True o xs opt rs
    f (ls:xs) opt rs | Just o <- readM ls = g False o xs opt rs
    f (ls:xs) opt rs | (nopt,[]) <- FO.process opt [map toLower ls] = f xs nopt rs
    f (ls:xs) opt rs = f xs opt (ls:xs)
    g inv o xs opt rs = case (Map.lookup o langMap,inv) of
        (Just (wh,True),False)  -> f xs ((opt Set.\\ languageFlags) `Set.union` wh) rs
        (Just (wh,True),True)   -> f xs opt (mno True o:rs)
        (Just (wh,False),False) -> f xs (opt `Set.union` wh) rs
        (Just (wh,False),True)  -> f xs (opt Set.\\ wh) rs
        (Nothing,i)             -> if o `Set.member` alwaysOn then f xs opt rs else f xs opt (mno i o:rs)
    mno False x = show x
    mno True x = 'N':'o':show x

readM :: (Monad m, Read a) => String -> m a
readM cs = case [x | (x,t) <-  reads cs, ("","") <- lex t] of
    [x] -> return x
    [] -> fail "readM: no parse"
    _ -> fail "readM: ambiguous parse"

-- these will be cleared by a language specification
languageFlags = Set.fromList
    [FO.BangPatterns
    ,FO.Ffi
    ,FO.Forall
    ,FO.Exists
    ,FO.MonomorphismRestriction
    ,FO.Prelude
    ,FO.Sugar
    ,FO.UserKinds
    ,FO.TypeFamilies
    ,FO.UnboxedTuples
    ,FO.UnboxedValues
    ,FO.Defaulting]

languageDefault = Set.fromList
    [FO.Defaulting
    ,FO.MonomorphismRestriction
    ,FO.Prelude
    ,FO.Sugar]

langMap = Map.fromList
    [CPP                       ==> FO.Cpp
    ,M4                        ==> FO.M4
    ,ForeignFunctionInterface  ==> FO.Ffi
    ,ImplicitPrelude           ==> FO.Prelude
    ,UnboxedTuples             ==> FO.UnboxedTuples
    ,UnboxedValues             ==> FO.UnboxedValues
    ,MonomorphismRestriction   ==> FO.MonomorphismRestriction
    ,ExplicitForAll            ==> FO.Forall
    ,ExistentialQuantification ==> FO.Forall
    ,ScopedTypeVariables       ==> FO.Forall
    ,Rank2Types                ==> FO.Forall
    ,RankNTypes                ==> FO.Forall
    ,BangPatterns              ==> FO.BangPatterns
    ,PolymorphicComponents     ==> FO.Forall
    ,TypeFamilies              ==> FO.TypeFamilies
    ,MagicHash                 =+> [FO.UnboxedValues, FO.UnboxedTuples]
    ,Haskell98       =-+> [FO.MonomorphismRestriction,FO.Prelude, FO.Sugar]
    ,Haskell2010     =-+> [FO.MonomorphismRestriction,FO.Prelude, FO.Sugar]]  where
    x ==> y = (x,(Set.singleton y,False)) -- maps to one flag
    x =+> y = (x,(Set.fromList y,False))  -- maps to set of flags
    x =-+> y = (x,(Set.fromList y,True))  -- erases all previous set flags

alwaysOn = Set.fromList
    [EmptyDataDecls
    ,NPlusKPatterns
    ,NamedFieldPuns
    ,KindSignatures
    ,LiberalTypeSynonyms
    ,NondecreasingIndentation
    ,RecordPuns
    ,TraditionalRecordSyntax
    ,TypeOperators
    ,TypeSynonymInstances]


{-# NOINLINE generateJhcParams #-}
generateJhcParams :: BS8.ByteString
generateJhcParams = BS8.pack $ unlines $ ["#ifndef _JHC_EXT_DEFS" ,"#define _JHC_EXT_DEFS"] 
    ++ map f (sort $ Set.toList alwaysOn ++  Map.keys langMap) ++ ["#endif"] where
        f x = "#define HS_EXT_" ++ show x ++ " 1"
        g (x,_) = f x

data KnownExtension
    = M4
    | UnboxedValues
    | Haskell2010
    | Haskell98

-- imported from ghc simply to provide better warnings.
    | AllowAmbiguousTypes | Arrows | AutoDeriveTypeable | BangPatterns | CApiFFI
    | CPP | ConstrainedClassMethods | ConstraintKinds | DataKinds
    | DatatypeContexts | DefaultSignatures | DeriveDataTypeable | DeriveFoldable
    | DeriveFunctor | DeriveGeneric | DeriveTraversable
    | DisambiguateRecordFields | DoAndIfThenElse | DoRec | EmptyCase
    | EmptyDataDecls | ExistentialQuantification | ExplicitForAll
    | ExplicitNamespaces | ExtendedDefaultRules | ExtensibleRecords
    | FlexibleContexts | FlexibleInstances | ForeignFunctionInterface
    | FunctionalDependencies | GADTSyntax | GADTs | GHCForeignImportPrim
    | GeneralizedNewtypeDeriving | Generics | HereDocuments | ImplicitParams
    | ImplicitPrelude | ImpredicativeTypes | IncoherentInstances | InstanceSigs
    | InterruptibleFFI | KindSignatures | LambdaCase | LiberalTypeSynonyms
    | MagicHash | MonadComprehensions | MonoLocalBinds | MonoPatBinds
    | MonomorphismRestriction | MultiParamTypeClasses | MultiWayIf
    | NPlusKPatterns | NamedFieldPuns | NegativeLiterals | NewQualifiedOperators
    | NondecreasingIndentation | NullaryTypeClasses | NumDecimals
    | OverlappingInstances | OverloadedLists | OverloadedStrings
    | PackageImports | ParallelArrays | ParallelListComp | PatternGuards
    | PatternSignatures | PolyKinds | PolymorphicComponents | PostfixOperators
    | QuasiQuotes | Rank2Types | RankNTypes | RebindableSyntax | RecordPuns
    | RecordWildCards | RecursiveDo | RegularPatterns | RelaxedPolyRec
    | RestrictedTypeSynonyms | RoleAnnotations | Safe | SafeImports
    | ScopedTypeVariables | StandaloneDeriving | TemplateHaskell
    | TraditionalRecordSyntax | TransformListComp | Trustworthy | TupleSections
    | TypeFamilies | TypeOperators | TypeSynonymInstances | UnboxedTuples
    | UndecidableInstances | UnicodeSyntax | UnliftedFFITypes | Unsafe
    | ViewPatterns | XmlSyntax
  deriving (Show, Read, Eq, Ord)
