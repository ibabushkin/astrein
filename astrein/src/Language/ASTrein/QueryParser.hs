module Language.ASTrein.QueryParser where

import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

-- | wrap a parser in parens.
parensParser :: Parser Query -> Parser Query
parensParser parser = char '(' *> skipSpace *> parser <* skipSpace <* char ')'

-- | a datatype representing a query, as it is parsed, with no semantic
-- information attached whatsoever.
data Query
    = QueryTerm Char [Text] -- ^ a single query term
    | QueryCombinator Char Query Query
    deriving (Eq, Show)

-- | parse a query from a textual representation
parseQuery :: Text -> Maybe Query
parseQuery = either (const Nothing) Just . parseOnly queryParser

-- | parse a single query term
termParser :: Parser Query
termParser = do
    sep <- anyChar
    body <- takeWhile1 (`notElem` [' ','(',')'])
    return $ QueryTerm sep (T.split (== sep) body)

-- | parse a linear chain of the same combinator
combinatorParser :: Parser Query
combinatorParser = do
    left <- recQueryParser
    sep <- getSeparator
    right <- recQueryParser `sepBy1` separator sep
    return $ foldl (QueryCombinator sep) left right
    where separator c = skipSpace *> char c *> skipSpace
          getSeparator = skipSpace *> anyChar <* skipSpace

-- | parse a query that can be used as part of a nested query.
recQueryParser :: Parser Query
recQueryParser = choice
    [ parensParser recQueryParser, parensParser combinatorParser, termParser]

-- | parse a query as it has been entered by the user.
queryParser :: Parser Query
queryParser = combinatorParser <|> recQueryParser
