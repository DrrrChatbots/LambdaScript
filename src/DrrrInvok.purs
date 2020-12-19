module DrrrInvok where

import BotScript
import Data.Boolean
import Data.Maybe
import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Foreign.Object as FO

foreign import invok :: String -> (Array Expr) -> Effect Unit
