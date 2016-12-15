{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Language.ASTrein.AST
    ( AST(..)
    , ParseResult
    , ASTMatches(..)
    , FileMatches
    , MatchOutput
    , parseAST
    , jsonParseAST
    , performMatch
    ) where

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO

import Language.ASTrein.QueryParser (RawQuery, parseQuery)

-- | a type representing the possibility of a failed file parsing
type ParseResult a = Either FilePath a

-- | a type to represent a set of matches for a query on a file
data ASTMatches a = ASTMatches
    { name :: FilePath -- ^ the file's name
    , content :: Text -- ^ the file's content
    , matches :: Maybe (QueryResult a) -- ^ the matches in the file
    }

-- | FileMatches are ASTMatches that can fail - if the file could not be
-- parsed into an AST
type FileMatches a = ParseResult (ASTMatches a)

-- | a MatchOutput is what gets returned by an action working on multiple files
-- and matching a query which possibly could not be parsed
type MatchOutput a = Maybe [FileMatches a]

-- | a typeclass associating a type for a language-specific AST with
-- a type used to query it and a way to obtain such queries from textual input
class AST a where
    -- | querying type
    data Query a :: *
    -- | query result type
    data QueryResult a :: *
    -- | check the validity of a query
    verifyQuery :: RawQuery -> Maybe (Query a)
    -- | parse a file into an AST
    parseAST' :: Text -> Maybe a
    -- | apply query to an AST
    match :: Query a -> a -> Maybe (QueryResult a)
    -- | render a query's result
    renderMatches :: ASTMatches a -> Text

-- | parse an AST and return the filename in case of failure
parseAST :: AST a => FilePath -> Text -> ParseResult a
parseAST fileName fileContent = transform $ parseAST' fileContent
    where transform (Just a) = Right a
          transform Nothing = Left fileName

-- | a simple implementation of parseAST' for JSON-backed AST machinery
jsonParseAST :: (AST a, FromJSON a) => Text -> Maybe a
jsonParseAST = decode . fromStrict . encodeUtf8

-- | match a query in textual represenation on an AST taken from a file
-- returns a wrapped Nothing on query parsing failure and a Nothing in the list
-- for each file that could not be parsed into an AST.
performMatch :: AST a => Text -> (FilePath -> IO Text) -> [FilePath] -> IO (MatchOutput a)
performMatch queryText getASTText files
    | Just query <- parseQuery queryText >>= verifyQuery = do
        contents <- mapM getASTText files
        let asts = zipWith parseAST files contents
        return . Just $ zipWith3 (transform query) files contents asts
    | otherwise = return Nothing
    where transform q fN fC = fmap (ASTMatches fN fC . match q)
