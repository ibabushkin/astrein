{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Data.Char (toLower)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Language.ASTrein.AST
import Language.ASTrein.AST.Haskell (HaskellAST)
import Language.ASTrein.AST.Simple (SimpleAST)
import Language.ASTrein.Display
import Language.ASTrein.Util (readMaybeStr)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr)

-- | supported languages type
data Language
    = Haskell
    | Simple
    deriving (Read, Eq)

-- | command line flag type
data Options = Options
    { language :: Language
    , nomatch :: Bool
    , query :: Maybe Text
    }

-- | command line options parsing specification
options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "l" ["language"]
        (ReqArg
            (\str opts ->
                case readMaybeStr str of
                  Just lang -> return opts { language = lang }
                  Nothing -> do
                      showError "warning: language not recognized, ignoring"
                      return opts
            )
            "LANGUAGE")
        "The language to use. Possible values are\n\
        \'Haskell' and 'Simple'. Default: 'Haskell'"
    , Option "q" ["query"]
        (ReqArg (\str opt -> return opt { query = Just (pack str) }) "QUERY")
        "The expression describing the query to be\n applied to the file(s).\n"
    , Option "n" ["nomatch"]
        (NoArg (\opts -> return opts { nomatch = True }))
        "Don't match the query, just parse and display the AST."
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                let header = prg ++ " version 0.1.0.0\nUSAGE: " ++
                        prg ++ " [OPTION..] file(s)\nOPTIONS:" 
                showError (pack $ usageInfo header options)
                exitSuccess))
        "Show this help."
    ]

-- | default options
defaultOptions :: Options
defaultOptions = Options Haskell False Nothing

-- | dispatch language to determine the computation necessary
dispatch :: Language -> Text -> [FilePath] -> IO [Text]
dispatch lang queryText files =
    case lang of
      Haskell -> (result :: IO (MatchOutput HaskellAST)) >>= render
      Simple -> (result :: IO (MatchOutput SimpleAST)) >>= render
    where result :: AST a => IO (MatchOutput a)
          result = performMatch queryText files
          render (Just a) = mapM renderFileMatches a
          render Nothing = crash "error: query parsing failed"

-- | dispatch a language to parse the files to ASTs and display them
dispatchAST :: Language -> [FilePath] -> IO [Text]
dispatchAST lang files =
    case lang of
      Haskell -> (asts :: IO (ASTOutput HaskellAST)) >>= mapM renderShow
      Simple -> (asts :: IO (ASTOutput SimpleAST)) >>= mapM renderShow
    where asts :: AST a => IO (ASTOutput a)
          asts = mapM parseAST files

-- | main routine
main :: IO ()
main = do
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    Options{..} <- foldl (>>=) (return defaultOptions) actions
    if nomatch
       then mapM_ TIO.putStrLn =<< dispatchAST language files
       else case query of
              Just q -> cleanPutStrLn =<<
                  T.intercalate "\n" <$> dispatch language q files
              Nothing -> crash "error: no query specified, aborting"

-- | clean output with only one newline at the end
cleanPutStrLn :: Text -> IO ()
cleanPutStrLn text
    | "\n" `T.isSuffixOf` text = TIO.putStr text
    | otherwise = TIO.putStrLn text
