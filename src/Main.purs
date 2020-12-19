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

test :: Array String
test = ["""
state main {
    title "hello"
    descr "hello world!"
    delay "20m"
    print "hello again"
    order "a song name"
    going hello
    x[2] = "asdf"
    x = 3
    x = -3.5
    x = [1,2,3,4]
    event left (user : "", msg : "^/play") {}
}
""", """
a = 2
state main {
 a = 2 + 3
}
"""]

-- main :: (Effect Unit)
-- main = for_ test (\ctx ->
--     parseShow parseScript ctx)

-- main = parseShow parseExpr "2 + 3"

ctx = """
  state hello {

  }
"""

btx = """
title "hello"
x = 3
state world {
    title "hello"
    descr "hello world!"
    delay "20m"
    print "hello again"
    delay "20m"
    order "a song name"
    delay "20m"
    x[2] = "asdf"
    x = 3
    x = -3.5
    x = [1,2,3,4]
    event left (user : "", msg : "^/play") {}
    going world
}
going world
"""

main = case parse parseScript btx of
    Right script -> runVM script
    Left err -> log ("error: " <> show err)

-- main = case parse parseScript btx of
--     Right script -> logShow script
--     Left err -> log ("error: " <> show err)
