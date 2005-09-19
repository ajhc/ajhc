module DataConstructors where


import E.E

data DataTable
followAliases :: DataTable -> E -> E
typesCompatable :: Monad m => DataTable -> E -> E -> m ()
