module DrrrBot where

import BotScript
import Data.Boolean
import Data.Maybe
import Data.Tuple.Nested

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Foreign.Object as FO

foreign import invok :: String -> (Array Expr) -> Effect Unit
foreign import listen :: String -> String -> (Array String) -> (Array String -> Effect Unit) -> Effect Unit
