{-# LANGUAGE OverloadedStrings #-}
import Language.ASTrein.Display (showError)

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitWith)
import System.Process (spawnProcess, waitForProcess)

-- | show a "helpful" message
showHelp :: IO ()
showHelp = do
    showError
        "astrein version 0.1.0.0\nUSAGE: astrein LANGUAGE QUERY FILE(S)"
    exitSuccess

-- | main routine
main :: IO ()
main = do
    args <- getArgs
    if length args < 3
       then showHelp
       else run args

-- | run a subcommand
run :: [String] -> IO ()
run (lang:args) =
    spawnProcess ("astrein-" ++ lang) args >>= waitForProcess >>= exitWith
