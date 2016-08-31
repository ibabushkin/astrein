{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Language.ASTrein.AST
import Language.ASTrein.AST.Haskell (HaskellAST)
import Language.ASTrein.AST.Simple (SimpleAST)
import Language.ASTrein.Util (readMaybeStr)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)

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

showError :: String -> IO ()
showError = hPutStrLn stderr

-- | crash with a message
crash :: String -> IO a
crash msg = showError msg >> exitFailure

-- | command line options parsing specification
options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "l" ["language"]
        (ReqArg
            (\str opts ->
                case readMaybeStr str of
                  Just lang -> return opts { language = lang }
                  Nothing -> do
                      hPutStrLn stderr
                          "warning: language not recognized, using default"
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
        "Don't match the query, just parse the AST."
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                let header = prg ++ " version 0.4\nUSAGE: " ++
                        prg ++ " [OPTION..] file(s)\nOPTIONS:" 
                hPutStrLn stderr (usageInfo header options)
                exitSuccess))
        "Show this help."
    ]

-- | default options
defaultOptions :: Options
defaultOptions = Options Haskell False Nothing

-- | show a parse error if necessary when dispatching an Either
parseError :: (a -> IO Text) -> Either String a -> IO Text
parseError func (Right r) = func r
parseError func (Left l) = do
    showError ("error: could not parse " ++ l ++ " to AST.")
    return mempty

-- | a type for easier dispatching of query results
type QueryOutput a = IO (Maybe [Either String (QueryResult a)])

-- | a type for easier dispatching of ASTs
type ASTOutput a = IO [Either String a]

-- | dispatch language to determine the computation necessary
dispatch :: Language -> Text -> [FilePath] -> IO [Text]
dispatch lang queryText files =
    case lang of
      Haskell -> show' (result :: QueryOutput HaskellAST)
      Simple -> show' (result :: QueryOutput SimpleAST)
    where result :: AST a => QueryOutput a
          result = perform queryText files
          show' res = do
              r <- res
              case r of
                Just a -> mapM render' a
                Nothing -> crash "error: query parsing failed"
          render' :: AST a => Either String (QueryResult a) -> IO Text
          render' = parseError render

-- | dispatch a language to parse the files to ASTs and display them
dispatchAST :: Language -> [FilePath] -> IO [Text]
dispatchAST lang files =
    case lang of
      Haskell -> (asts :: ASTOutput HaskellAST) >>= mapM show'
      Simple -> (asts :: ASTOutput SimpleAST) >>= mapM show'
    where asts :: AST a => IO [Either String a]
          asts = mapM parseAST files
          show' :: Show a => Either String a -> IO Text
          show' = parseError (return . pack . show)

-- | main routine
main :: IO ()
main = do
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    Options{..} <- foldl (>>=) (return defaultOptions) actions
    if nomatch
       then mapM_ TIO.putStrLn =<< dispatchAST language files
       else case query of
              Just q -> mapM_ TIO.putStrLn =<<
                  intersperse "===\n" <$> dispatch language q files
              Nothing -> crash "error: no query specified, aborting"
