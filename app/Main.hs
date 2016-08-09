{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
import Data.Char (toLower)
import Data.Text (Text, pack)

import Language.ASTrein.AST
import Language.ASTrein.AST.Haskell (HaskellAST)
import Language.ASTrein.AST.Simple (SimpleAST)
import Language.ASTrein.Util (readMaybeStr)

import System.Console.GetOpt
import System.Environment (getArgs)

data Language
    = Haskell
    | Simple
    deriving (Read, Eq)

data Options = Options
    { language :: Language
    , query :: Text
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option "l" ["language"]
        (ReqArg
            (\str opts ->
                case readMaybeStr str of
                  Just lang -> opts { language = lang }
                  Nothing -> opts
            )
            "LANGUAGE")
        "The language to use. Possible values are\n\
        \'Haskell' and 'Simple'. Default: 'Haskell'"
    , Option "qp" ["query", "pattern"]
        (ReqArg (\str opt -> opt { query = pack str }) "QUERY")
        "The expression describing the query to be applied to the file(s).\n"
    ]

defaultOptions :: Options
defaultOptions = Options Haskell mempty

dispatch :: Language -> Text -> [FilePath] -> IO [String]
dispatch Haskell queryText files = map show <$>
    (perform queryText files :: IO [Maybe (QueryResult HaskellAST)])
dispatch Simple queryText files = map show <$>
    (perform queryText files :: IO [Maybe (QueryResult SimpleAST)])

main :: IO ()
main = do
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    let Options{..} = foldl (flip ($)) defaultOptions actions
    mapM_ putStrLn =<< dispatch language query files
