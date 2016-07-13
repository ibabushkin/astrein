{-# LANGUAGE OverloadedStrings, TypeFamilies, ViewPatterns #-}
module Language.ASTrein.AST.Simple where

import Language.ASTrein.AST

import Data.Attoparsec.Text
import Data.Text (Text)

newtype SimpleAST = SimpleAST ()

-- | a type, given by name
newtype TypeName = TypeName Text
    deriving (Show, Read, Eq)

-- | a toplevel value/function, given by name
newtype ValueName = ValueName Text
    deriving (Show, Read, Eq)

newtype LineNumber = LineNumber Integer
    deriving (Show, Read, Eq)

-- | a query
data SimpleQuery
    = TypeIdent TypeName -- ^ query for a type name
    | ValueIdent ValueName -- ^ query for a value name
    | Nest SimpleQuery SimpleQuery
    -- ^ reduce the search-space using the first query
    | Range SimpleQuery SimpleQuery -- ^ return the range between matches
    | LineIdent LineNumber -- ^ get the object at a given linenumber
    deriving (Show, Read, Eq)

instance AST SimpleAST where
    data Query SimpleAST = SimpleQuery
    -- every query matches for demonstration purposes
    match ast _ = ast
    parsers = Parsers
        { elements = [elementParser '.' (ValueIdent . ValueName)]
        , chains = []
        }
