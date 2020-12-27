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
event msg (user : "", msg : "喵") { drrr.print("汪")};
"""

execute ctx = case parse parseScript ctx of
    Right script -> runVM script
    Left err -> log ("error: " <> show err)

compile ctx = case parse parseScript ctx of
    Right script -> logShow script
    Left err -> log ("error: " <> show err)

main = log "Welcome to use BotScript"
-- m = 0
-- main = (if m == 0 then compile else execute) ctx
