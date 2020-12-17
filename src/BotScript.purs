module BotScript where

import Data.Array
import Data.Tuple.Nested
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Var = Var String (Array Expr)

instance showVar :: Show Var where
  show (Var s ns) = "(Var " <> show s <> " " <> show ns <> ")"

data Expr
  = Arr (Array Expr)
  | BinOp Expr Expr
  | UnaOp String
  | Str String
  | Num Number
  | Ref Var

instance showExpr :: Show Expr where
  show (BinOp l r)
    = "(BinOp "
    <> show l <> " "
    <> show r <> ")"
  show (UnaOp e) = "(UnaOp " <> " " <> show e <> ")"
  show (Num   n) = show n
  show (Str   s) = show s
  show (Ref   s) = "(Ref " <> show s <> ")"
  show (Arr  xs) = show xs

data ETypes
  = Join
  | Exit
  | Play
  | Over
  | Milk
  | Talk
  | Priv

derive instance genericETypes :: Generic ETypes _
instance showETypes :: Show ETypes where
  show = genericShow

type Period = String
type Rule = String /\ String

data Action
  = Title Expr
  | Descr Expr
  | Delay Expr
  | Print Expr
  | Order Expr
  | Going String
  -- | Cases [...]
  -- | Sleep Period
  | Renew Var Expr
  | Timer Period Action
  | Group (Array Action)
  | While Expr Action
  | Visit Var Expr Action
  | Match Expr Action Action
  | Event ETypes (Array Rule) Action

instance showAction :: Show Action where
  show (Title cont) = "(Title " <> show cont <> ")"
  show (Descr cont) = "(Descr " <> show cont <> ")"
  show (Delay time) = "(Delay " <> show time <> ")"
  show (Print cont) = "(Print " <> show cont <> ")"
  show (Order song) = "(Order " <> show song <> ")"
  show (Going stat) = "(Going " <> show stat <> ")"
  show (Renew lval rvalue)
    = "(Renew " <> show lval <> " " <> show rvalue <> ")"
  show (Timer time action)
    = "(Timer " <> show time <> " " <> show action <> ")"
  show (Group actions)
    = "(Group " <> show actions <> ")"
  show (While expr action)
    = "(While "
    <> show expr <> " "
    <> show action <> ")"
  show (Visit var expr action)
    = "(Visit "
    <> show var <> " "
    <> show expr <> " "
    <> show action <> ")"
  show (Match prd thn els)
    = "(Match "
       <> show prd <> " "
       <> show thn <> " "
       <> show els <> ")"
  show (Event name rules action)
    = "(Event "
       <> show name <> " "
       <> show rules <> " "
       <> show action <> ")"

data BotState = BotState String Action
instance showBotState :: Show BotState where
  show (BotState name action) =
    "(BotState " <> name <> " " <> show action <> ")"

data BotScript = BotScript (Array Action) (Array BotState)
instance showBotScript :: Show BotScript where
  show (BotScript bindings states) =
    "(BotScript " <> show bindings <> " " <> show states <> ")"
