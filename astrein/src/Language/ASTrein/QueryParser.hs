module Language.ASTrein.QueryParser where

import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

{-
   We are parsing queries in an incredibly simple fashion.
   Each query consists of one or more alternatives. These are separated
   by the string " | ".
-}

parensParser :: Parser Query -> Parser Query
parensParser parser = char '(' *> skipSpace *> parser <* skipSpace <* char ')'

data Query
    = QueryToken Char [Text]
    | QueryCombinator Char Query Query
    deriving (Eq, Show)

parseQuery :: Text -> Maybe Query
parseQuery = either (const Nothing) Just . parseOnly queryParser

parseQuery' = parseOnly queryParser

tokenParser :: Parser Query
tokenParser = do
    sep <- anyChar
    body <- takeWhile1 (`notElem` [' ','(',')'])
    return $ QueryToken sep (T.split (== sep) body)

combinatorParser :: Parser Query
combinatorParser = do
    left <- recQueryParser
    skipSpace
    sep <- anyChar
    skipSpace
    right <- recQueryParser
    return $ QueryCombinator sep left right

recQueryParser :: Parser Query
recQueryParser = choice
    [ parensParser recQueryParser, parensParser combinatorParser, tokenParser]

queryParser :: Parser Query
queryParser = combinatorParser <|> recQueryParser
