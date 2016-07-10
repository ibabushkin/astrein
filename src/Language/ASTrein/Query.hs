module Language.ASTrein.Query where

-- | a name that can be queried for
data Name
    = TypeName String -- ^ a type, given by name
    | ValueName String -- ^ a toplevel value/function, given by name
    deriving (Show, Read, Eq)

-- | a query
data Query
    = Ident Name -- ^ query for a name
    | Nest Query Query -- ^ reduce the search-space using the first query
    | Range Query Query -- ^ return the range between matches
    | LineNumber Integer -- ^ get the object at a given linenumber
    deriving (Show, Read, Eq)
