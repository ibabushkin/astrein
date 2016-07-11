{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Language.ASTrein.Query where

import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

import Language.ASTrein.Util (readMaybe)

-- | a type, given by name
newtype TypeName = TypeName Text
    deriving (Show, Read, Eq)

-- | a toplevel value/function, given by name
newtype ValueName = ValueName Text
    deriving (Show, Read, Eq)

newtype LineNumber = LineNumber Integer
    deriving (Show, Read, Eq)

-- | a query
data Query
    = TypeIdent TypeName -- ^ query for a type name
    | ValueIdent ValueName -- ^ query for a value name
    | Nest Query Query -- ^ reduce the search-space using the first query
    | Range Query Query -- ^ return the range between matches
    | LineIdent LineNumber -- ^ get the object at a given linenumber
    deriving (Show, Read, Eq)

-- | parse a name, the basic building block of a query
typeParser :: Parser TypeName
typeParser = char ':' *> (TypeName <$> name)
    where name = takeWhile1 (`notElem` (" ()" :: String))

valueParser :: Parser ValueName
valueParser = char '.' *> (ValueName <$> name)
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

-- | things that need/have parens - parser
complexParser :: Parser Query
complexParser = char '(' *> skipSpace *> guts <* skipSpace <* char ')'
    where guts = nestParser <|> rangeParser <|> lineNumParser

-- | ident parser
identParser :: Parser Query
identParser = (TypeIdent <$> typeParser) <|> (ValueIdent <$> valueParser)

-- | nest parser
nestParser :: Parser Query
nestParser = Nest <$> queryParser <*> (" . " *> queryParser)

-- | range parser
rangeParser :: Parser Query
rangeParser = Range <$> queryParser <*> (" - " *> queryParser)

-- | line num parser
lineNumParser :: Parser Query
lineNumParser = (LineIdent . LineNumber) <$> decimal

-- | parse a Text object into a Query
parseQuery :: Text -> Maybe Query
parseQuery = either (const Nothing) Just . parseOnly toplevelParser

-- | RPN query parser
queryParserRPN :: Parser [Query]
queryParserRPN = foldl go [] <$> sepBy (takeWhile1 (/= ' ')) space
    where go :: [Query] -> Text -> [Query]
          go (t2:t1:ts) "." = Nest t1 t2 : ts
          go (t2:t1:ts) "-" = Range t1 t2 : ts
          go ts (T.uncons -> Just ('.', n)) = ValueIdent (ValueName n) : ts
          go ts (T.uncons -> Just (':', n)) = TypeIdent (TypeName n) : ts
          go ts (readMaybe -> Just ln) = LineIdent ln : ts

-- | parse a query in RPN syntax
parseQueryRPN :: Text -> Maybe Query
parseQueryRPN input =
    case parseOnly queryParserRPN input of
      Right [q] -> Just q
      _ -> Nothing
