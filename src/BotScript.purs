module BotScript where

import Data.Array
import Data.Tuple.Nested
import Foreign
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.List as L
import Effect (Effect)
import Effect.Exception (name)
import Undefined (undefined)

type Term = (Record (toString :: Foreign -> String))
foreign import toTerm :: forall a. String -> a -> Term
foreign import stringify_ :: forall a. a -> String

type Args = Array (String /\ Expr)

data Expr
  = Trm Term
  | Una String Expr
  | Bin String Expr Expr
  | Var String
  | Obj (Array (String /\ Expr))
  | Abs Args Expr
  | App Expr (Array Expr)
  | Sub Expr Expr
  | Arr (Array Expr)
  | Dot Expr String
  | Later Expr Expr
  | Going String
  | Visit String
  | Reset String
  | Renew Expr Expr -- note lval
  | Timer Expr Expr
  | Group (List Expr)
  | While Expr Expr
  | Ifels Expr Expr Expr
  | Event (Array String) Expr
  -- | Cases [...]
  -- | Sleep Period

instance showExpr :: Show Expr where
  show (Arr  xs) = show xs
  show (Bin o l r)
    = "(Bin "
    <> show o <> " "
    <> show l <> " "
    <> show r <> ")"
  show (Una o e)
    = "(Una "
    <> show o <> " "
    <> show e <> ")"
  show (App expr args)
    = "(App "
    <> show expr <> " "
    <> show args <> ")"
  show (Abs args expr)
    = "(Abs "
    <> show args <> " "
    <> show expr <> ")"
  show (Dot expr attr)
    = show expr <> "." <> show attr
  show (Sub expr sub)
    = show expr <> "[" <> show sub <> "]"
  show (Var   s) = "(Var " <> show s <> ")"
  show (Obj   pairs) = "(Obj " <> show pairs <> ")"
  show (Trm   term) = stringify_ term

  show (Later time expr)
    = "(Later "
    <> show time <> " "
    <> show expr <> ")"
  show (Going stat) = "(Going " <> show stat <> ")"
  show (Visit stat) = "(Visit " <> show stat <> ")"
  show (Reset stat) = "(Reset " <> show stat <> ")"
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
  show (Ifels prd thn els)
    = "(Ifels "
       <> show prd <> " "
       <> show thn <> " "
       <> show els <> ")"
  show (Event name expr)
    = "(Event "
       <> show name <> " "
       <> show expr <> ")"

  show _ = "undefined"

data BotState = BotState String Expr
instance showBotState :: Show BotState where
  show (BotState name action) =
    "(BotState " <> name <> " " <> show action <> ")"

data BotScript = BotScript (List Expr) (Array BotState)
instance showBotScript :: Show BotScript where
  show (BotScript actions states) =
    "(BotScript " <> show actions <> " " <> show states <> ")"
