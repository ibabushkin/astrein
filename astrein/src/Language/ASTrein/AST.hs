{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Language.ASTrein.AST where

import Control.Applicative ((<|>))

import Data.Attoparsec.Text hiding (match)
import Data.Text (Text)
import qualified Data.Text as T

-- | a type representing the possibility of a failed file parsing
type ParseResult a = Either FilePath a

-- | a type to represent a set of matches for a query on a file
data ASTMatches a = ASTMatches
    { name :: FilePath -- ^ the file's name
    , matches :: QueryResult a -- ^ the matches in that file
    }

-- | FileMatches are ASTMatches that can fail - if the file could not be
-- parsed into an AST
type FileMatches a = ParseResult (ASTMatches a)

-- | a MatchOutput is what gets returned by an action working on multiple files
-- and matching a query which possibly could not be parsed
type MatchOutput a = Maybe [FileMatches a]

-- | an ASTOutput is what gets returred by an action working on multiple files
-- and parsing their AST's
type ASTOutput a = [ParseResult a]

-- | a typeclass associating a type for a language-specific AST with
-- a type used to query it and a way to obtain such queries from textual input
class AST a where
    -- | querying type
    data Query a :: *
    -- | query result type
    data QueryResult a :: *
    -- | parse a file into an AST
    parseAST' :: FilePath -> IO (Maybe a)
    -- | all parsers needed to parse a `Text` into a `Query a`
    queryParsers :: Parsers a
    -- | apply query to an AST
    match :: Query a -> a -> QueryResult a
    -- | render a query's result
    renderMatches :: ASTMatches a -> IO Text

-- | parse an AST and return the filename in case of failure
parseAST :: AST a => FilePath -> IO (ParseResult a)
parseAST file = transform <$> parseAST' file
    where transform (Just a) = Right a
          transform Nothing = Left file

-- | parse a query from text
parseQuery :: AST a => Text -> Maybe (Query a)
parseQuery = either (const Nothing) Just . parseOnly toplevelParser

-- | match a query in textual represenation on an AST taken from a file
-- returns a wraped Nothing on query parsing failure and a Nothing in the list
-- for each file that could not be parsed to an AST.
performMatch :: AST a => Text -> [FilePath] -> IO (MatchOutput a)
performMatch queryText files
    | Just query <- parseQuery queryText = do
        asts <- mapM parseAST files
        return . Just $ zipWith (transform query) files asts
    | otherwise = return Nothing
    where transform query fName = fmap (ASTMatches fName . match query)

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
    where name = takeWhile1 pred
          pred a = a `notElem` (" ()" :: String) && a /= T.head c

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
nestedParser =
    choice ((mappend <$> map brace . chains <*> elements) queryParsers)
    where brace p = char '(' *> skipSpace *> p <* skipSpace <* char ')'

-- | the toplevel parser of the generic query grammar
toplevelParser :: AST a => Parser (Query a)
toplevelParser = (choice (chains queryParsers) <|> nestedParser) <* endOfInput
