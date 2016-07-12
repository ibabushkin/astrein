{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Language.ASTrein.AST where

import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Text (Text)

-- | a typeclass associating a type for a language-specific AST with
-- a type used to query it and a way to obtain such queries from textual input
class AST a where
    -- | querying type
    data Query a :: *
    -- | apply query
    match :: a -> Query a -> a
    -- | all parsers needed to parse a `Text` into a `Query a`
    parsers :: Parsers a
    -- | parse a query from text
    parseQuery :: Text -> Maybe (Query a)

data Parsers a = Parsers
    { elements :: [Parser (Query a)]
    , chains :: [Parser (Query a)]
    }

-- | generate a parser for an atomic query for a named object, for instance
-- a type, class or value.
elementParser :: AST a
              => Char
              -> (Text -> Query a)
              -> Parser (Query a)
elementParser c cons = char c *> (cons <$> name)
    where name = takeWhile1 (`notElem` (" ()" :: String))

lineNumParser :: AST a => (Integer -> Query a) -> Parser (Query a)
lineNumParser = (<$> decimal)

chainingParser :: AST a
               => Text
               -> (Query a -> Query a -> Query a)
               -> Parser (Query a)
chainingParser sep cons =
    cons <$> toplevelParser <*> (string sep *> toplevelParser)

toplevelParser :: AST a => Parser (Query a)
toplevelParser = choice ((mappend <$> map brace . chains <*> elements) parsers)
    <* endOfInput
    where brace p = char '(' *> skipSpace *> p <* skipSpace <* char ')'

