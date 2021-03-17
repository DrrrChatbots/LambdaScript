module Main where

import BotScript
import BotScriptEnv
import BotScriptParser
import BotScriptVM
import Data.Either
import Prelude

import Data.Array.ST.Iterator (next)
import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log, logShow)
import Undefined (undefined)

newMachine :: forall a. a -> MachineState
newMachine x = { val: none undefined
               , cur: ""
               , env: pushEnv Top
               , exprs: (Nil : Nil)
               , states: []
               }

execute ctx = case parse parseScript ctx of
    Right script -> do
       runVM script
    Left err -> do
       log ("error: " <> show err)
       pure $ { val: none undefined
              , cur: ""
              , env: Top
              , exprs: Nil
              , states: []
              }

interact machine ctx =
    case parse parseScript ctx of
         Right script -> do
            runStep machine script
         Left err -> do
            log ("error: " <> show err)
            pure $ machine

compile ctx = case parse parseScript ctx of
    Right script -> logShow script
    Left err -> log ("error: " <> show err)

execute' ctx = do
    machine <- execute ctx
    log $ "=> " <> stringify_ machine.val

main = log "Welcome to use LambdaScript"
