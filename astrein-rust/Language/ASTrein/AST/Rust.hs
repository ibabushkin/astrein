{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternGuards, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.ASTrein.AST.Rust (RustAST(..)) where

import Data.Aeson

import GHC.Generics

import Language.ASTrein.AST

-- | a Rust AST (lies!)
newtype RustAST = RustAST ()
    deriving (Generic, Show, Eq)

instance FromJSON RustAST

instance AST RustAST where
    data Query RustAST = QName ()
        deriving (Show, Eq)
    data QueryResult RustAST = QResult ()
        deriving (Show, Eq)
    verifyQuery = const Nothing
    parseAST' = jsonParseAST
    match _ = const Nothing
    renderMatches = mempty
