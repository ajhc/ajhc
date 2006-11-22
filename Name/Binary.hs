module Name.Binary() where

import Maybe
import Data.Monoid

import Binary
import Name.Id
import Name.Name


instance Binary IdSet where
    put_ bh ids = do
        putNList bh [ id | id <- idSetToList ids, isNothing (fromId id)]
        putNList bh [ n | id <- idSetToList ids, n <- fromId id]
    get bh = do
        (idl:: [Id])   <- getNList bh
        (ndl:: [Name]) <- getNList bh
        return (idSetFromDistinctAscList idl `mappend` idSetFromList (map toId ndl))


instance Binary a => Binary (IdMap a) where
    put_ bh ids = do
        putNList bh [ x | x@(id,_) <- idMapToList ids, isNothing (fromId id)]
        putNList bh [ (n,v) | (id,v) <- idMapToList ids, n <- fromId id]
    get bh = do
        idl <- getNList bh
        ndl <- getNList bh
        return (idMapFromDistinctAscList idl `mappend` idMapFromList [ (toId n,v) | (n,v) <- ndl ])

