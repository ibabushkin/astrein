{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Language.ASTrein.Util (Dispatcher, dispatchMatch, languageMain) where

import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Language.ASTrein.AST
import Language.ASTrein.Display

import System.Console.GetOpt
import System.Environment (getArgs)

-- | a phantom-type wrapper for Text representing queries
newtype QueryText a = QueryText Text

-- | a type representing functions performing actions for a specific language
type Dispatcher a = QueryText a -> [FilePath] -> IO [Text]

-- | a generic Dispatcher
dispatchMatch :: forall a. AST a => Dispatcher a
dispatchMatch (QueryText queryText) files =
    (performMatch queryText files :: IO (MatchOutput a)) >>= render
    where render (Just a) = mapM renderFileMatches a
          render Nothing = crash "error: query parsing failed"

-- | a main function to use in a subprogram for a specific language
languageMain :: AST a => Dispatcher a -> IO ()
languageMain dispatch = do
    (query:files) <- getArgs
    T.intercalate "\n" <$> dispatch (QueryText $ pack query) files
        >>= cleanPutStrLn

-- | clean output with only one newline at the end
cleanPutStrLn :: Text -> IO ()
cleanPutStrLn text
    | "\n" `T.isSuffixOf` text = TIO.putStr text
    | otherwise = TIO.putStrLn text
