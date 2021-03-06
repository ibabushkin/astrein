{-# LANGUAGE PatternSynonyms #-}
module Language.ASTrein.QueryParser
    ( RawQuery(..)
    , pattern FuncName
    , pattern TypeName
    , pattern ClassName
    , pattern Instance
    , pattern Range
    , pattern Nest
    , pattern Or
    , pattern Named
    , parseQuery
    ) where

import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

-- | wrap a parser in parens.
parensParser :: Parser RawQuery -> Parser RawQuery
parensParser parser = char '(' *> skipSpace *> parser <* skipSpace <* char ')'

-- | a datatype representing a query, as it is parsed, with no semantic
-- information attached whatsoever.
data RawQuery
    = QueryTerm Char [Text] -- ^ a single query term
    | QueryCombinator Char RawQuery RawQuery -- ^ a combinator of two queries
    deriving (Eq, Show)

-- | our default for function names: a (sub)query, containing no spaces or
-- parens, prefixed by a dot.
pattern FuncName :: Text -> RawQuery
pattern FuncName n = QueryTerm '.' [n]
-- | our default for type names: a (sub)query, containing no spaces or parens,
-- prefixed by a colon.
pattern TypeName :: Text -> RawQuery
pattern TypeName n = QueryTerm ':' [n]
-- | our default for class/interface names: a (sub)query, containing no
-- spaces or parens, prefixed by a comma.
pattern ClassName :: Text -> RawQuery
pattern ClassName n = QueryTerm ',' [n]
-- | our default for class/interface instances: a (sub)query, containing no
-- spaces or parens, of the form ";classname;typename".
pattern Instance :: Text -> Text -> RawQuery
pattern Instance c n = QueryTerm ';' [c,n]

-- | our default for a range combinator: a (sub)query, which separates two
-- subqueries by a dash and some surrounding whitespace.
pattern Range :: RawQuery -> RawQuery -> RawQuery
pattern Range s e = QueryCombinator '-' s e
-- | our default for a nesting combinator: a (sub)query, which separates two
-- subqueries by a dot and some surrounding whitespace.
pattern Nest :: RawQuery -> RawQuery -> RawQuery
pattern Nest t s = QueryCombinator '.' t s
-- | our default for alternative patterns: a (sub)query, which separates two
-- subqueries by a pipe and some surrounding whitespace.
pattern Or :: RawQuery -> RawQuery -> RawQuery
pattern Or l r = QueryCombinator '|' l r

-- | our default pattern for "named things": a (sub)query, which still
-- consists of a single `QueryTerm`.
pattern Named :: Text -> Text -> RawQuery
pattern Named n t = QueryTerm '/' [n, t]

-- | parse a query from a textual representation
parseQuery :: Text -> Maybe RawQuery
parseQuery = either (const Nothing) Just . parseOnly queryParser

-- | parse a single query term
termParser :: Parser RawQuery
termParser = do
    sep <- anyChar
    body <- takeWhile1 (`notElem` [' ','(',')'])
    return $ QueryTerm sep (T.split (== sep) body)

-- | parse a linear chain of the same combinator
combinatorParser :: Parser RawQuery
combinatorParser = do
    left <- recQueryParser
    sep <- getSeparator
    right <- recQueryParser `sepBy1` separator sep
    return $ foldl (QueryCombinator sep) left right
    where separator c = skipSpace *> char c *> skipSpace
          getSeparator = skipSpace *> anyChar <* skipSpace

-- | parse a query that can be used as part of a nested query.
recQueryParser :: Parser RawQuery
recQueryParser = choice
    [ parensParser recQueryParser, parensParser combinatorParser, termParser]

-- | parse a query as it has been entered by the user.
queryParser :: Parser RawQuery
queryParser = combinatorParser <|> recQueryParser
