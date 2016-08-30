{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Language.ASTrein.AST where

import Control.Applicative ((<|>))

import Data.Attoparsec.Text hiding (match)
import Data.Text (Text)

-- | a typeclass associating a type for a language-specific AST with
-- a type used to query it and a way to obtain such queries from textual input
class AST a where
    -- | querying type
    data Query a :: *
    -- | query result type
    data QueryResult a :: *
    -- | parse a file into an AST
    parseAST :: FilePath -> IO (Maybe a)
    -- | apply query to an AST
    match :: Query a -> a -> QueryResult a
    -- | render a query's result
    render :: QueryResult a -> IO Text
    -- | all parsers needed to parse a `Text` into a `Query a`
    parsers :: Parsers a

-- | parse a query from text
parseQuery :: AST a => Text -> Maybe (Query a)
parseQuery = either (const Nothing) Just . parseOnly toplevelParser

-- | match a query in textual represenation on an AST taken from a file
-- returns a wraped Nothing on query parsing failure and a Nothing in the list
-- for each file that could not be parsed to an AST.
perform :: AST a => Text -> [FilePath] -> IO (Maybe [Maybe (QueryResult a)])
perform queryText files
    | Just query <- parseQuery queryText = do
        files <- mapM parseAST files
        return . Just $ map (fmap (match query)) files
    | otherwise = return Nothing

-- | a collection type for all elementar and derived parsers for a Query type
data Parsers a = Parsers
    { elements :: [Parser (Query a)] -- ^ elementar parsers
    , chains :: [Parser (Query a)] -- ^ derived parsers
    }

-- | generate a parser for an atomic query for a named object, for instance
-- a type, class or value.
elementParser :: AST a
              => Text
              -> (Text -> Query a)
              -> Parser (Query a)
elementParser c cons = string c *> (cons <$> name)
    where name = takeWhile1 (`notElem` (" ()" :: String))

-- | generate a parser for an atomic query for a named object with a name
-- consisting of two parts.
element2Parser :: AST a
               => Text
               -> (Text -> Text -> Query a)
               -> Parser (Query a)
element2Parser c cons = string c *> (cons <$> name <*> (string c *> name))
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
