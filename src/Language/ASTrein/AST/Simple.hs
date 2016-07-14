{-# LANGUAGE OverloadedStrings, TypeFamilies, ViewPatterns #-}
module Language.ASTrein.AST.Simple where

import Language.ASTrein.AST

import Data.Attoparsec.Text
import Data.Text (Text)

newtype SimpleAST = SimpleAST ()
    deriving (Show, Read, Eq)

-- | a type, given by name
newtype TypeName = TypeName Text
    deriving (Show, Read, Eq)

-- | a toplevel value/function, given by name
newtype ValueName = ValueName Text
    deriving (Show, Read, Eq)

newtype LineNumber = LineNumber Integer
    deriving (Show, Read, Eq)

instance AST SimpleAST where
    data Query SimpleAST
        = TypeIdent TypeName
        | ValueIdent ValueName
        | Nest (Query SimpleAST) (Query SimpleAST)
        | Range (Query SimpleAST) (Query SimpleAST)
        | LineIdent LineNumber
        deriving (Show, Eq)
    -- every query matches for demonstration purposes
    match ast _ = ast
    parsers = Parsers
        { elements = [ elementParser '.' (ValueIdent . ValueName)
                     , elementParser ':' (TypeIdent . TypeName)
                     , lineNumParser (LineIdent . LineNumber)
                     ]
        , chains = [ chainingParser " . " Nest
                   , chainingParser " - " Range
                   ]
        }
