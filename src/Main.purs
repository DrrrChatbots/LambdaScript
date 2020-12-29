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
print("beg")
state hello {
    print("hello")
}

hello = {
    print("call procedure hello")
}

visit hello

print("end")
"""

execute ctx = case parse parseScript ctx of
    Right script -> runVM script
    Left err -> log ("error: " <> show err)

compile ctx = case parse parseScript ctx of
    Right script -> logShow script
    Left err -> log ("error: " <> show err)

-- main = log "Welcome to use BotScript"
m = 1
main = (if m == 0 then compile else execute) ctx
