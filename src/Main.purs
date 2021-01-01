module Main where

import BotScript
import BotScriptEnv
import BotScriptParser
import BotScriptVM
import Data.Either
import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log, logShow)

ctx = """
timer 1000 (a) => print("hello world");
"""

execute ctx = case parse parseScript ctx of
    Right script -> do
       runVM script
    Left err -> log ("error: " <> show err)

compile ctx = case parse parseScript ctx of
    Right script -> logShow script
    Left err -> log ("error: " <> show err)

main = log "Welcome to use BotScript"
-- m = 1
-- main = (if m == 0 then compile else execute) ctx
