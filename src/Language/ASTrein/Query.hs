{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Language.ASTrein.Query where

import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

import Language.ASTrein.Util (readMaybe)

-- | a name that can be queried for
data Name
    = TypeName Text -- ^ a type, given by name
    | ValueName Text -- ^ a toplevel value/function, given by name
    deriving (Show, Read, Eq)

-- | a query
data Query
    = Ident Name -- ^ query for a name
    | Nest Query Query -- ^ reduce the search-space using the first query
    | Range Query Query -- ^ return the range between matches
    | LineNumber Integer -- ^ get the object at a given linenumber
    deriving (Show, Read, Eq)

-- | parse a name, the basic building block of a query
nameParser :: Parser Name
nameParser =
    (char ':' *> (TypeName <$> name)) <|> (char '.' *> (ValueName <$> name))
    where name = takeWhile1 (`notElem` (" ()" :: String))

-- | toplevel query
toplevelParser :: Parser Query
toplevelParser = choice
    [ nestParser
    , rangeParser
    , identParser
    , lineNumParser
    , complexParser
    ] <* endOfInput

-- | atomic parser of subquery
queryParser :: Parser Query
queryParser = identParser <|> complexParser

-- | things that need parens - parser
complexParser :: Parser Query
complexParser = char '(' *> skipSpace *> guts <* skipSpace <* char ')'
    where guts = nestParser <|> rangeParser <|> lineNumParser

-- | ident parser
identParser :: Parser Query
identParser = Ident <$> nameParser

-- | nest parser
nestParser :: Parser Query
nestParser = Nest <$> queryParser <*> (" . " *> queryParser)

-- | range parser
rangeParser :: Parser Query
rangeParser = Range <$> queryParser <*> (" - " *> queryParser)

-- | line num parser
lineNumParser :: Parser Query
lineNumParser = LineNumber <$> decimal

-- | parse a Text object into a Query
parseQuery :: Text -> Maybe Query
parseQuery = either (const Nothing) Just . parseOnly toplevelParser

-- | RPN query parser
queryParserRPN :: Parser [Query]
queryParserRPN = foldl go [] <$> sepBy (takeWhile1 (/= ' ')) space
    where go :: [Query] -> Text -> [Query]
          go (t2:t1:ts) "." = Nest t1 t2 : ts
          go (t2:t1:ts) "-" = Range t1 t2 : ts
          go ts (T.uncons -> Just ('.', n)) = Ident (ValueName n) : ts
          go ts (T.uncons -> Just (':', n)) = Ident (TypeName n) : ts
          go ts (readMaybe -> Just ln) = LineNumber ln : ts

-- | parse a query in RPN syntax
parseQueryRPN :: Text -> Maybe Query
parseQueryRPN input =
    case parseOnly queryParserRPN input of
      Right [q] -> Just q
      _ -> Nothing
