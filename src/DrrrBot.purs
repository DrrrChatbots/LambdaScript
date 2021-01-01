module DrrrBot where

import BotScript
import Data.Boolean
import Data.Maybe
import Data.Tuple.Nested

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Foreign.Object as FO

foreign import listen :: String -> (Array String) -> Array Term -> Term -> Effect Unit
foreign import unlisten :: String -> Effect Unit
foreign import clearAllEvent :: Effect Unit

