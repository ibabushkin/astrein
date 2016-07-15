{-# LANGUAGE OverloadedStrings, TypeFamilies, ViewPatterns #-}
module Language.ASTrein.AST.Template where

import Language.ASTrein.AST

import Data.Attoparsec.Text
import Data.Text (Text)

-- | a value identifier element
valueParser :: AST a => (Text -> Query a) -> Parser (Query a)
valueParser = elementParser '.'

-- | a type identifier element
typeParser :: AST a => (Text -> Query a) -> Parser (Query a)
typeParser = elementParser ':'

-- | a class identifier element
classParser :: AST a => (Text -> Query a) -> Parser (Query a)
classParser = elementParser ','

-- | a line number element
lineNumParser :: AST a => (Integer -> Query a) -> Parser (Query a)
lineNumParser = (<$> decimal)
