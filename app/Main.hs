{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
import Data.Char (toLower)
import Data.Text (Text, pack)

import Language.ASTrein.AST
import Language.ASTrein.AST.Haskell (HaskellAST)
import Language.ASTrein.AST.Simple (SimpleAST)
import Language.ASTrein.Util (readMaybeStr)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn, stderr)

-- | supported languages type
data Language
    = Haskell
    | Simple
    deriving (Read, Eq)

-- | command line flag type
data Options = Options
    { language :: Language
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
                      hPutStrLn stderr
                          "error: language not recognized, using default"
                      return opts
            )
            "LANGUAGE")
        "The language to use. Possible values are\n\
        \'Haskell' and 'Simple'. Default: 'Haskell'"
    , Option "q" ["query"]
        (ReqArg (\str opt -> return opt { query = Just (pack str) }) "QUERY")
        "The expression describing the query to be\n applied to the file(s).\n"
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
defaultOptions = Options Haskell Nothing

-- | a type for easier dispatching of actions
type ActionResult a = IO (Maybe [Maybe (QueryResult a)])

-- | dispatch language to determine computation necessary
dispatch :: Language -> Text -> [FilePath] -> IO [String]
dispatch lang queryText files =
    case lang of
      Haskell -> show' (result :: ActionResult HaskellAST)
      Simple -> show' (result :: ActionResult SimpleAST)
    where result :: (AST a, Show (QueryResult a)) => ActionResult a
          result = perform queryText files
          show' res = do
              r <- res
              case r of
                Just a -> return $ map show a
                Nothing -> do
                    hPutStrLn stderr "error: query parsing failed"
                    exitSuccess
                    return []

-- | main routine
main :: IO ()
main = do
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    Options{..} <- foldl (>>=) (return defaultOptions) actions
    case query of
      Just q -> mapM_ putStrLn =<< dispatch language q files
      Nothing -> hPutStrLn stderr "error: no query specified, aborting"
