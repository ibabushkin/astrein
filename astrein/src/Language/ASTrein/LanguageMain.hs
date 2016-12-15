{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Language.ASTrein.LanguageMain
    ( Dispatcher
    , dispatchMatch
    , languageMain
    ) where

import Data.Aeson (FromJSON)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Language.ASTrein.AST
import Language.ASTrein.Display

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)

-- | a phantom-type wrapper for Text representing queries
newtype QueryText a = QueryText Text

-- | an type representing the data gathered from command line options
data Options a = Options
    { query :: Maybe (QueryText a) -- ^ a possibly missing query
    , verbose :: Bool -- ^ verbosity information
    }

-- | default values for all options
defaultOptions :: Options a
defaultOptions = Options Nothing False

-- | type of our options function, only defined to save some typing below
type Opt a = [OptDescr (Options a -> IO (Options a))]

-- | option parsing template for all language executables
options :: forall a. AST a => Opt a
options =
    [ Option "q" ["query"]
        (ReqArg (\q opts -> return opts { query = Just . QueryText $ pack q })
        "QUERY")
        "The query the AST is to be matched on.\n\
        \If no such argument is present, dump the AST instead of\n\
        \matching a query."
    , Option "h" ["help"]
        (NoArg (\_ -> do
            prg <- getProgName
            let header = prg ++ " 1.0.0.0\nUSAGE: " ++
                    prg ++ " [OPTION(S)] FILE(S)\nOPTIONS:"
            showError (pack $ usageInfo header (options :: Opt a))
            exitSuccess))
        "Show this help."
    , Option "v" ["verbose"]
        (NoArg (\opts -> return opts { verbose = True }))
        "If passed, more (some) output for non-matches is generated."
    ]

-- | a type representing functions performing actions for a specific language
type Dispatcher a = Options a -> [FilePath] -> IO [Text]

-- | a generic Dispatcher
dispatchMatch :: forall a. (AST a, Show a) => (FilePath -> IO Text) -> Dispatcher a
dispatchMatch getASTText (Options (Just (QueryText queryText)) verb) files =
    (performMatch queryText getASTText files :: IO (MatchOutput a)) >>= render
    where render (Just a) = filter (/= mempty) <$> mapM (renderFileMatches verb) a
          render Nothing = crash "error: query parsing failed"
dispatchMatch getASTText _ files = do
    contents <- mapM getASTText files
    return $ map render (zipWith parseAST files contents :: [ParseResult a])
    where render = pack . show

-- | a main function to use in a subprogram for a specific language
languageMain :: AST a => Dispatcher a -> IO ()
languageMain dispatch = do
    args <- getArgs
    let (actions, files, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    if null errors
       then do
           str <- T.intercalate "\n" <$> dispatch opts files
           cleanPutStrLn str
       else mapM_ putStrLn errors

-- | clean output with only one newline at the end
cleanPutStrLn :: Text -> IO ()
cleanPutStrLn text
    | "\n" `T.isSuffixOf` text = TIO.putStr text
    | otherwise = TIO.putStrLn text
