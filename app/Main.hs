{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
import Data.Char (toLower)
import Data.List (intersperse)
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
                let header = prg ++ " version 0.1.0.0\nUSAGE: " ++
                        prg ++ " [OPTION..] file(s)\nOPTIONS:" 
                hPutStrLn stderr (usageInfo header options)
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
      Haskell -> (result :: IO (MatchOutput HaskellAST)) >>= show'
      Simple -> (result :: IO (MatchOutput SimpleAST)) >>= show'
    where result :: AST a => IO (MatchOutput a)
          result = performMatch queryText files
          show' (Just a) = mapM renderFileMatches a
          show' Nothing = crash "error: query parsing failed"

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
              Just q -> TIO.putStrLn =<<
                  T.intercalate "\n" <$> dispatch language q files
              Nothing -> crash "error: no query specified, aborting"
