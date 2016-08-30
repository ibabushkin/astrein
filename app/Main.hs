{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
import Data.Char (toLower)
import Data.Text (Text, pack)
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
        "Don't match the query, only parse the AST"
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

-- | a type for easier dispatching of actions
type ActionResult a = IO (Maybe [Maybe (QueryResult a)])

-- | dispatch language to determine the computation necessary
dispatch :: Language -> Text -> [FilePath] -> IO [Text]
dispatch lang queryText files =
    case lang of
      Haskell -> show' (result :: ActionResult HaskellAST)
      Simple -> show' (result :: ActionResult SimpleAST)
    where result :: AST a => ActionResult a
          result = perform queryText files
          show' res = do
              r <- res
              case r of
                Just a -> mapM render' a
                Nothing -> crash "error: query parsing failed"
          render' :: AST a => Maybe (QueryResult a) -> IO Text
          render' (Just r) = render r
          render' Nothing =
              showError "error: AST could not be parsed" >> return mempty

-- | dispatch a language to parse the files to ASTs and display them
dispatchAST :: Language -> [FilePath] -> IO [Text]
dispatchAST lang files =
    case lang of
      Haskell -> (asts :: IO [Maybe HaskellAST]) >>= mapM show'
      Simple -> (asts :: IO [Maybe SimpleAST]) >>= mapM show'
    where asts :: AST a => IO [Maybe a]
          asts = mapM parseAST files
          show' r = case r of
                      Just a -> return . pack $ show a
                      Nothing -> crash "error: AST parsing failed"

-- | main routine
main :: IO ()
main = do
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    Options{..} <- foldl (>>=) (return defaultOptions) actions
    if nomatch
       then mapM_ TIO.putStrLn =<< dispatchAST language files
       else case query of
              Just q -> mapM_ TIO.putStr =<< dispatch language q files
              Nothing -> crash "error: no query specified, aborting"
