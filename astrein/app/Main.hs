{-# LANGUAGE OverloadedStrings #-}
import Language.ASTrein.Display (showError)

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (pack)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure, exitWith)
import System.Process (spawnProcess, waitForProcess)

-- | show a help message
showHelp :: Bool -> IO a
showHelp success = do
    prg <- getProgName
    let header = prg ++ " 0.4.0.0\nUSAGE: " ++ prg ++
            " [SUBCOMMAND [SUBCOMMAND-OPTION(S)] FILE(S)|OPTION(S)]\nOPTIONS:"
    showError (pack $ usageInfo header options)
    if success then exitSuccess else exitFailure

-- | which mode are we running in?
data Mode = Help | List | Subcommand

-- | option parsing specification
options :: [OptDescr Mode]
options =
    [ Option "l" ["list", "languages"] (NoArg List)
        "List available subcommands/languages instead of calling a subcommand."
    , Option "h" ["help"] (NoArg Help)
        "Show this help."
    ]

-- | main routine
main :: IO ()
main = do
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    interpret args . fromMaybe Subcommand . listToMaybe $ reverse actions

-- | dispatch mode
interpret :: [String] -> Mode -> IO ()
interpret args Subcommand = run args
interpret _ Help = showHelp True
interpret _ List = undefined

-- | run a subcommand
run :: [String] -> IO ()
run [] = showHelp False
run (lang:args) =
    spawnProcess ("astrein-" ++ lang) args >>= waitForProcess >>= exitWith
