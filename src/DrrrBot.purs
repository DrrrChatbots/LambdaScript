module DrrrBot where

import BotScript
import Data.Boolean
import Data.Maybe
import Data.Tuple.Nested

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)

foreign import listen :: String -> (Array String) -> Array Term -> Term -> Effect Unit
foreign import setcur :: String -> Effect Unit
foreign import clearAllEvent :: Effect Unit

