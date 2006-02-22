module DataConstructors where


import E.E

data DataTable
followAliases :: DataTable -> E -> E
followAlias :: Monad m => DataTable -> E -> m E
typesCompatable :: Monad m => DataTable -> E -> E -> m ()
