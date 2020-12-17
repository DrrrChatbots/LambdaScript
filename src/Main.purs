module Main where

import BotScriptVM (runVM)
import BotScriptParser

import Prelude (Unit)
import Effect (Effect)

ctx :: String
ctx = """
state hello {
    title "hello"
    descr "hello world!"
    delay "20m"
    print "hello again"
    order "a song name"
    going hello
    x[2] = "asdf"
    x = 3
    x = 3.5
    event left (user : "", msg : "^/play") {}
}

state world {
    x[2] = "asdf"
    x = 3
    x = 3.5
}
"""

main :: Effect Unit
main = let script = parse parseScript ctx
        in runVM "main" script []
