module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import BotScriptParser
import Effect.Console (logShow)


-- main :: Effect Unit
main = parse """state hello {  title "hello world" }"""
