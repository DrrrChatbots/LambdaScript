module Main where

import BotScript (stringify_)
import BotScriptEnv (Env(..))
import BotScriptParser (parse, parseScript)
import BotScriptVM (
  MachineState, newObject, none,
  runStep, runVM, rawMachine, wrapMachine)
import Data.Either (Either(..))
import Data.List (List(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Prelude (Unit, bind, discard, pure, show, ($), (<>))
import Undefined (undefined)

newMachine :: forall a. a -> MachineState
newMachine x = wrapMachine (rawMachine x)

execute :: String -> Effect MachineState
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
              , events: newObject undefined
              , timers: newObject undefined
              }

interact :: MachineState -> String -> Effect MachineState
interact machine ctx =
    case parse parseScript ctx of
         Right script -> do
            runStep machine script
         Left err -> do
            log ("error: " <> show err)
            pure $ machine

compile :: String -> Effect Unit
compile ctx = case parse parseScript ctx of
    Right script -> logShow script
    Left err -> log ("error: " <> show err)

execute' :: String -> Effect Unit
execute' ctx = do
    machine <- execute ctx
    log $ "=> " <> stringify_ machine.val

main :: Effect Unit
main = log "Welcome to use LambdaScript"
