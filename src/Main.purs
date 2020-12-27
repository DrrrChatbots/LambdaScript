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
for(i = 0; i < 10; i++){
    print(i);
}

state test {
    Math = [1,2,3,4]
    Math.line = 4
    print(Math)
    print(Math.line)
}
state world {
    title("hello")
    title(true   )
}
x[2] = 3
state fuck {
    title(true)
    title(-1)
    title("hello" + " world")
    descr("hello world!")
    delay("20m")
    print("hello again")
    delay("20m")
    order("a song name")
    delay("20m")
    x[2] = "asdf"
    x = 3
    x = -3.5
    x = [1,2,3,4]
    event left (user : "", msg : "^/play") {}
    going fuck
}
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
