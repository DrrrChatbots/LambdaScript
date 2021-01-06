module Test.Main where

import BotScript
import BotScriptEnv
import BotScriptParser
import BotScriptVM
import Data.Either
import Prelude

import Data.Array.ST.Iterator (next)
import Data.List (List(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Undefined (undefined)

ctx = """
a = new Set([1,2,3])
"""
execute ctx = case parse parseScript ctx of
    Right script -> do
       runVM script
       -- log $ machine.val.toString undefined
    Left err -> do
       log ("error: " <> show err)
       pure $ { val: none undefined
              , cur: ""
              , env: Top
              , exprs: Nil
              , states: []
              }

compile ctx = case parse parseScript ctx of
    Right script -> logShow script
    Left err -> log ("error: " <> show err)

execute' ctx = do
    machine <- execute ctx
    log $ "=> " <> stringify_ machine.val

m = 1
main = (if m == 0 then compile else execute') ctx
