module Name.Binary() where

import Data.Monoid
import Maybe

import Data.Binary
import GenUtil(replicateM)
import Name.Id
import Name.Internals
import Support.MapBinaryInstance

instance Binary IdSet where
    put ids = do
        putList [ id | id <- idSetToList ids, isNothing (fromId id)]
        putList [ n | id <- idSetToList ids, n <- fromId id]
    get = do
        (idl:: [Id])   <- getList
        (ndl:: [Name]) <- getList
        return (idSetFromDistinctAscList idl `mappend` idSetFromList (map toId ndl))

instance Binary a => Binary (IdMap a) where
    put ids = do
        putList [ x | x@(id,_) <- idMapToList ids, isNothing (fromId id)]
        putList [ (n,v) | (id,v) <- idMapToList ids, n <- fromId id]
    get = do
        idl <- getList
        ndl <- getList
        return (idMapFromDistinctAscList idl `mappend` idMapFromList [ (toId n,v) | (n,v) <- ndl ])
