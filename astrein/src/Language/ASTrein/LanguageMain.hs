{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Language.ASTrein.LanguageMain
    ( Dispatcher
    , dispatchMatch
    , languageMain
    ) where

import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Monoid((<>))

import Language.ASTrein.AST
import Language.ASTrein.Display

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)

-- | a phantom-type wrapper for Text representing queries
newtype QueryText a = QueryText Text

-- | an type representing the data gathered from command line options
data Options a = Options
    { query :: Maybe (QueryText a)
    }

-- | type of our options function, only defined to safe some typing below
type Opt a = [OptDescr (Options a -> IO (Options a))]

-- | option parsing template for all language executables
options :: forall a. AST a => Opt a
options =
    [ Option "q" ["query"]
        (ReqArg (\q opts -> return opts { query = Just . QueryText $ pack q })
        "QUERY")
        "The query the AST is to be matched on."
    , Option "h" ["help"]
        (NoArg (\_ -> do
            prg <- getProgName
            let header = prg ++ " 0.1.0.0\nUSAGE: " ++
                    prg ++ " [OPTION(S)] FILE(S)\nOPTIONS:"
            showError (pack $ usageInfo header (options :: Opt a))
            exitSuccess))
        "Show this help."
    ]

-- | a type representing functions performing actions for a specific language
type Dispatcher a = Maybe (QueryText a) -> [FilePath] -> IO [Text]

-- | a generic Dispatcher
dispatchMatch :: forall a. AST a => Dispatcher a
dispatchMatch (Just (QueryText queryText)) files =
    (performMatch queryText files :: IO (MatchOutput a)) >>= render
    where render (Just a) = mapM renderFileMatches a
          render Nothing = crash "error: query parsing failed"
dispatchMatch Nothing _ = crash "todo: AST dumping yet to be implemented"

-- | a main function to use in a subprogram for a specific language
languageMain :: AST a => Dispatcher a -> IO ()
languageMain dispatch = do
    args <- getArgs
    let (actions, files, errors) = getOpt RequireOrder options args
    Options{..} <- foldl (>>=) (return (Options Nothing :: Options a)) actions
    if length errors == 0
       then do
           str <- T.intercalate "\n" <$> dispatch query files
           cleanPutStrLn str
       else mapM_ putStrLn errors
    {-case args of
      [] -> do
          prg <- pack <$> getProgName
          showError $ "astrein-" <> prg <> " version 0.1.0.0\n\
              \USAGE: astrein-haskell QUERY FILE(S)"
      (query:files) -> do
          str <- T.intercalate "\n" <$> dispatch (QueryText $ pack query) files
          cleanPutStrLn str
          -}

-- | clean output with only one newline at the end
cleanPutStrLn :: Text -> IO ()
cleanPutStrLn text
    | "\n" `T.isSuffixOf` text = TIO.putStr text
    | otherwise = TIO.putStrLn text
