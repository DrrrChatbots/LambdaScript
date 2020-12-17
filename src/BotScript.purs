module BotScript where

import Data.Array
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Expr
  = BinOp Expr Expr
  | UnaOp String
  | Num Number

instance showExpr :: Show Expr where
  show (BinOp l r) = "(BinOp " <> show l <> " " <> show r <> ")"
  show (UnaOp e) = "(UnaOp " <> " " <> show e <> ")"
  show (Num   n) = "(Num " <> show n <> ")"

data VarRef = VarRef String (Array Number)

instance showVarRef :: Show VarRef where
  show (VarRef s ns) = "(VarRef " <> show s <> " " <> show ns <> ")"

data ETypes
  = Join
  | Left
  | Play
  | Over
  | Milk
  | Talk
  | Priv

derive instance genericETypes :: Generic ETypes _
instance showETypes :: Show ETypes where
  show = genericShow

type Period = String

data Action
  = Title String
  | Descr String
  | Delay Period
  | Print String
  | Order String
  | Going String
  -- | Match [...]
  -- | Sleep Period
  -- | Renew VarRef Expr
  | Timer Period Action
  | Event ETypes Action
  | Group (Array Action)

instance showAction :: Show Action where
  show (Title cont) = "(Title " <> show cont <> ")"
  show (Descr cont) = "(Descr " <> show cont <> ")"
  show (Delay time) = "(Delay " <> show time <> ")"
  show (Print cont) = "(Print " <> show cont <> ")"
  show (Order song) = "(Order " <> show song <> ")"
  show (Going stat) = "(Going " <> show stat <> ")"
  show (Timer time action)
    = "(Timer " <> show time <> " " <> show action <> ")"
  show (Event name action)
    = "(Event " <> show name <> " " <> show action <> ")"
  show (Group actions)
    = "(Group " <> show actions <> ")"

data BotState = BotState String Action
instance showBotState :: Show BotState where
  show (BotState name action) =
    "(BotState " <> name <> " " <> show action <> ")"

data BotScript = BotScript String Action
instance showBotScript :: Show BotScript where
  show (BotScript name action) =
    "(BotScript " <> name <> " " <> show action <> ")"
