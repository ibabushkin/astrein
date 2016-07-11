{-# LANGUAGE TypeFamilies #-}
module Language.ASTrein.AST where

import Data.Text (Text)

-- | a typeclass associating a type for a language-specific AST with
-- a type used to query it and a way to obtain such queries from textual input
class AST a where
    -- | querying type
    data Query a :: *
    -- | apply query
    match :: a -> Query a -> a
    -- | parse a query from text
    parseQuery :: Text -> Maybe (Query a)
