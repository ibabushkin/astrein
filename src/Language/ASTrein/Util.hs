module Language.ASTrein.Util (readMaybe, readMaybeStr) where

import Data.Maybe (listToMaybe)
import Data.Text (Text, unpack)

readMaybe :: Read a => Text -> Maybe a
readMaybe = readMaybeStr . unpack

readMaybeStr :: Read a => String -> Maybe a
readMaybeStr = fmap fst . listToMaybe . reads
