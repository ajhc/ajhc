module DataConstructors where


import E.E
import Name.Name

data DataTable
followAliases :: DataTable -> E -> E
followAlias :: Monad m => DataTable -> E -> m E
typesCompatable :: Monad m => E -> E -> m ()
updateLit :: DataTable -> Lit e t -> Lit e t
slotTypes :: DataTable -> Name -> E -> [E]
mktBox :: E -> E
