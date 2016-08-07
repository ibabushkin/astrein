{-# LANGUAGE ExistentialQuantification, FlexibleContexts, RecordWildCards #-}
import Data.Char (toLower)
import Data.Text (Text, pack)

import Language.ASTrein.AST
import Language.ASTrein.AST.Haskell (runHaskell)
import Language.ASTrein.AST.Simple (runSimple)

import System.Console.GetOpt
import System.Environment (getArgs)

data Options = forall a. Show (QueryResult a) => Options
    { func :: FilePath -> Text -> IO (QueryResult a)
    , query :: Text
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option "l" ["language"]
        (ReqArg
            (\str Options{..} ->
                case map toLower str of
                  "haskell" -> Options
                      { func = runHaskell
                      , query = query
                      }
                  "simple" -> Options
                      { func = runSimple
                      , query = query
                      })
            "LANGUAGE")
        "The language to use. Possible values are\n\
        \'Haskell' and 'Simple'. Default: 'Haskell'"
    , Option "qp" ["query", "pattern"]
        (ReqArg
            (\str opt -> opt { query = pack str })
            "QUERY")
        "The expression describing the query to be applied to the file(s).\n"
    ]

defaultOptions :: Options
defaultOptions = Options runHaskell mempty

main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
      (actions, files, []) -> do
          let opts = foldl (flip ($)) defaultOptions actions
          return ()
      _ -> return ()
