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
    -- | apply query to an AST
    match :: a -> Query a -> a
    -- | all parsers needed to parse a `Text` into a `Query a`
    parsers :: Parsers a

-- | parse a query from text
parseQuery :: AST a => Text -> Maybe (Query a)
parseQuery = either (const Nothing) Just . parseOnly toplevelParser

-- | a collection type for all elementar and derived parsers for a Query type
data Parsers a = Parsers
    { elements :: [Parser (Query a)] -- ^ elementar parsers
    , chains :: [Parser (Query a)] -- ^ derived parsers
    }

-- | generate a parser for an atomic query for a named object, for instance
-- a type, class or value.
elementParser :: AST a
              => Char
              -> (Text -> Query a)
              -> Parser (Query a)
elementParser c cons = char c *> (cons <$> name)
    where name = takeWhile1 (`notElem` (" ()" :: String))

-- | combinator to allow for chaining two parsers together with an infix
-- to construct a nested query.
chainingParser :: AST a
               => Text
               -> (Query a -> Query a -> Query a)
               -> Parser (Query a)
chainingParser sep cons =
    cons <$> nestedParser <*> (string sep *> nestedParser)

-- | a element in a nested context
nestedParser :: AST a => Parser (Query a)
nestedParser = choice ((mappend <$> map brace . chains <*> elements) parsers)
    where brace p = char '(' *> skipSpace *> p <* skipSpace <* char ')'

-- | the toplevel parser of the generic query grammar
toplevelParser :: AST a => Parser (Query a)
toplevelParser = (choice (chains parsers) <|> nestedParser) <* endOfInput
