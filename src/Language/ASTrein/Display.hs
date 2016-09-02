{-# LANGUAGE OverloadedStrings #-}
module Language.ASTrein.Display where

import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Data.Monoid ((<>))

import Language.ASTrein.AST

import System.Exit (exitFailure)
import System.IO (stderr)

-- | show a message on stderr
showError :: Text -> IO ()
showError = TIO.hPutStrLn stderr

-- | crash with a message
crash :: Text -> IO a
crash msg = showError msg >> exitFailure

-- | render FileMatches using the AST typeclass' renderMatches function
renderFileMatches :: AST a => FileMatches a -> IO Text
renderFileMatches = renderParseResult renderMatches

-- | render a ParseResult wrapping a type implementing Show
renderShow :: Show a => ParseResult a -> IO Text
renderShow = renderParseResult (return . pack . show)

-- | render a ParseResult give a rendering function for the underlying type
renderParseResult :: (a -> IO Text) -> ParseResult a -> IO Text
renderParseResult _ (Left file) = do
    showError $ "error: could not parse " <> pack file <> " to an AST"
    return mempty
renderParseResult renderFunc (Right a) = renderFunc a
