{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Language.ASTrein.QueryParser (parseQuery, RawQuery(..))

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- main spec
spec :: Spec
spec = describe "Language.ASTrein.QueryParser" parseQuerySpec

data ValidQuery = ValidQuery Text RawQuery
    deriving (Eq, Show)

instance Arbitrary ValidQuery where
    arbitrary = (ValidQuery <$> renderQuery <*> id) <$> arbitrary

instance Arbitrary RawQuery where
    arbitrary = oneof
        [ do sep <- suchThat arbitrary (`notElem` [' ', '(', ')'])
             QueryTerm sep <$> listOf1 (suchThat arbitrary
                 (\v -> T.filter (`notElem` [' ', '(', ')', sep]) v == v &&
                     v /= mempty))
        , QueryCombinator <$> suchThat arbitrary (/= ' ') <*>
            arbitrary <*> arbitrary
        ]

renderQuery :: RawQuery -> Text
renderQuery (QueryTerm c ts) = sep <> mconcat (intersperse sep ts)
    where sep = T.singleton c
renderQuery (QueryCombinator c l r) = "(" <> renderQuery l <> ") " <>
    T.singleton c <> " (" <> renderQuery r <> ")"

parseQuerySpec :: Spec
parseQuerySpec = describe "parseQuery" $
    it "parses valid inputs correctly" $ property $
        \(ValidQuery s q) -> parseQuery s == Just q
