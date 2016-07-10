module Language.ASTrein.Util (readMaybe) where

import Data.Maybe (listToMaybe)
import Data.Text (Text, unpack)

readMaybe :: Read a => Text -> Maybe a
readMaybe = fmap fst . listToMaybe . reads . unpack
