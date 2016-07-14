{-# LANGUAGE OverloadedStrings, TypeFamilies, ViewPatterns #-}
module Language.ASTrein.AST.Simple where

import Language.ASTrein.AST

import Data.Attoparsec.Text
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
    -- | every query matches for obvious reasons
    match ast _ = ast
    -- | all our parsers
    parsers = Parsers
        { elements = [ elementParser '.' (ValueIdent . ValueName)
                     , elementParser ':' (TypeIdent . TypeName)
                     , lineNumParser (LineIdent . LineNumber)
                     ]
        , chains = [ chainingParser " . " Nest
                   , chainingParser " - " Range
                   ]
        }
