module Language.ASTrein.Query where

import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

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
nameParser = (char ':' *> (TypeName <$> takeWhile1 (/= ' '))) <|>
    (char '.' *> (ValueName <$> takeWhile1 (/= ' ')))

-- | toplevel query
toplevelParser :: Parser Query
toplevelParser = (identParser <|> nestParser <|> rangeParser <|>
    lineNumParser <|> complexParser) <* endOfInput

-- | atomic parser of subquery
queryParser :: Parser Query
queryParser = identParser <|> complexParser

-- | things that need parens
complexParser :: Parser Query
complexParser = char '(' *> skipSpace *> guts <* skipSpace <* char ')'
    where guts = nestParser <|> rangeParser <|> lineNumParser

-- | parse an ident
identParser :: Parser Query
identParser = Ident <$> nameParser

-- | parse a nesting of two queries
nestParser :: Parser Query
nestParser = Nest <$> queryParser <*> (char ' ' *> queryParser)

-- | parse a range of two queries
rangeParser :: Parser Query
rangeParser = Range <$> queryParser <*> (char '-' *> queryParser)

lineNumParser :: Parser Query
lineNumParser = LineNumber <$> decimal

parseQuery :: Text -> Maybe Query
parseQuery = maybeResult . parse toplevelParser
