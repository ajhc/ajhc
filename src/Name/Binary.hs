module Name.Binary() where

import Maybe
import Data.Monoid

import Data.Binary
import Name.Id
import Name.Name


instance Binary IdSet where
    put ids = do
        put [ id | id <- idSetToList ids, isNothing (fromId id)]
        put [ n | id <- idSetToList ids, n <- fromId id]
    get = do
        (idl:: [Id])   <- get
        (ndl:: [Name]) <- get
        return (idSetFromDistinctAscList idl `mappend` idSetFromList (map toId ndl))


instance Binary a => Binary (IdMap a) where
    put ids = do
        put [ x | x@(id,_) <- idMapToList ids, isNothing (fromId id)]
        put [ (n,v) | (id,v) <- idMapToList ids, n <- fromId id]
    get = do
        idl <- get
        ndl <- get
        return (idMapFromDistinctAscList idl `mappend` idMapFromList [ (toId n,v) | (n,v) <- ndl ])

