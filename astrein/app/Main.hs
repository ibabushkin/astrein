{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Data.Char (toLower)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Language.ASTrein.AST
import Language.ASTrein.Display

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitWith)
import System.Process (spawnProcess, waitForProcess)

showHelp :: IO ()
showHelp = do
    showError
        "astrein version 0.1.0.0\nUSAGE: astrein LANGUAGE [OPTION(S)] FILE(S)"
    exitSuccess

-- | main routine
main :: IO ()
main = do
    args <- getArgs
    if length args < 3
       then showHelp
       else run args

run :: [String] -> IO ()
run (lang:args) =
    spawnProcess ("astrein-" ++ lang) args >>= waitForProcess >>= exitWith
