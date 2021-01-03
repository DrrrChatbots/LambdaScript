module Main where

import BotScript
import BotScriptEnv
import BotScriptParser
import BotScriptVM
import Data.Either
import Prelude

import Data.Array.ST.Iterator (next)
import Data.Foldable (for_)
import Data.List (List(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Undefined (undefined)

ctx = """
//@k = 3;
//obj = [1,2,3,4]
//iter = obj.values()
//for(it = iter.next(); !it.done; it = iter.next()){
//	print(it);
//}
a = [1,2,3,4]
for(i in a) print(i)
for(i of a) print(i)
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
    log $ machine.val.toString undefined

main = log "Welcome to use BotScript"
-- m = 1
-- main = (if m == 0 then compile else execute') ctx
