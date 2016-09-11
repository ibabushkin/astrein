{-# LANGUAGE OverloadedStrings #-}
module Language.ASTrein.Display where

import Data.Text (Text, pack)
import Data.Text.IO (hPutStrLn)

import Data.Maybe (isJust)
import Data.Monoid ((<>))

import Language.ASTrein.AST

import System.Exit (exitFailure)
import System.IO (stderr)

-- | show a message on stderr
showError :: Text -> IO ()
showError = hPutStrLn stderr

-- | crash with a message
crash :: Text -> IO a
crash msg = showError msg >> exitFailure

-- | render FileMatches using the AST typeclass' renderMatches function
renderFileMatches :: AST a => Bool -> FileMatches a -> IO Text
renderFileMatches verbose ms
    | verbose || matchPresent ms = renderParseResult renderMatches ms
    | otherwise = return mempty
    where matchPresent (Right ms) = isJust (matches ms)
          matchPresent _ = False


-- | render a ParseResult wrapping a type implementing Show
renderShow :: Show a => ParseResult a -> IO Text
renderShow = renderParseResult (return . pack . show)

-- | render a ParseResult give a rendering function for the underlying type
renderParseResult :: (a -> IO Text) -> ParseResult a -> IO Text
renderParseResult _ (Left file) = do
    showError $ "error: could not parse " <> pack file <> " to an AST"
    return mempty
renderParseResult renderFunc (Right a) = renderFunc a
