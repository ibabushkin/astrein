{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Language.ASTrein.AST.Simple (SimpleAST(..)) where

import Language.ASTrein.AST
import Language.ASTrein.AST.Template

import Data.Text (Text)

-- | trivial AST for demonstration purposes
newtype SimpleAST = SimpleAST ()
    deriving (Show, Read, Eq)

-- | a type, given by name
newtype TypeName = TypeName Text
    deriving (Show, Read, Eq)

-- | a toplevel value/function, given by name
newtype ValueName = ValueName Text
    deriving (Show, Read, Eq)

-- | a line number
newtype LineNumber = LineNumber Integer
    deriving (Show, Read, Eq)

-- | implement the AST typeclass for the SimpleAST type
instance AST SimpleAST where
    -- | the associated query type
    data Query SimpleAST
        = TypeIdent TypeName
        | ValueIdent ValueName
        | Nest (Query SimpleAST) (Query SimpleAST)
        | Range (Query SimpleAST) (Query SimpleAST)
        | LineIdent LineNumber
        deriving (Show, Eq)
    -- | the associated query result type
    data QueryResult SimpleAST = Match
        deriving (Show, Eq)
    -- | parsing non-existant AST's is surprisingly simple
    parseAST' _ = return . Just . SimpleAST $ ()
    -- | all our parsers
    queryParsers = Parsers
        { elements = [ valueParser (ValueIdent . ValueName)
                     , typeParser (TypeIdent . TypeName)
                     , lineNumParser (LineIdent . LineNumber)
                     ]
        , chains = [ chainingParser " . " Nest
                   , chainingParser " - " Range
                   ]
        }
    -- | every query matches for obvious reasons
    match _ _ = Match
    -- | showing trivial matches
    renderMatches _ = return "Trivial Match\n"