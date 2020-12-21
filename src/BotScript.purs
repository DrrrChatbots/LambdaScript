module BotScript where

import Data.Array
import Data.Tuple.Nested
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.List as L
import Effect.Exception (name)

data Var = Var String (Array Expr)

instance showVar :: Show Var where
  show (Var s ns) = "(Var " <> show s <> " " <> show ns <> ")"

data Expr
  = Arr (Array Expr)
  | Bin String Expr Expr
  | Una String Expr
  | Str String
  | Num Number
  | Ref Var
  | Obj (Record (toString :: String))
  | Null

instance showExpr :: Show Expr where
  show (Bin o l r)
    = "(Bin "
    <> show o <> " "
    <> show l <> " "
    <> show r <> ")"
  show (Una o e)
    = "(Una "
    <> show o <> " "
    <> show e <> ")"
  show (Num   n) = show n
  show (Str   s) = show s
  show (Ref   s) = "(Ref " <> show s <> ")"
  show (Arr  xs) = show xs
  show (Obj obj) = obj.toString
  show Null = "Null"

type ConsT =
    { array :: Array Expr -> Expr
    , number :: Number -> Expr
    , object :: { toString :: String
                }
                -> Expr
    , string :: String -> Expr
    , undefined :: Expr
    }

type ToExprT = forall a. ConsT -> a -> Expr
foreign import toExprFFI :: ToExprT

consTab =  { array: Arr
        , string: Str
        , number: Num
        , object: Obj
        , undefined: Null
        }


toExpr :: forall a. a -> Expr
toExpr = toExprFFI consTab

type Period = String
type Rule = String /\ String


-- Invok : Title Descr Delay Print Order Going
data Action
  = Invok String (Array Expr)
  -- | Cases [...]
  -- | Sleep Period
  | Delay Expr
  | Going String
  | Renew Var Expr
  | Timer Period Action
  | Group (List Action)
  | While Expr Action
  | Visit Var Expr Action
  | Match Expr Action Action
  | Event String (Array Rule) Action

instance showAction :: Show Action where
  show (Invok name args)
    = "(Invok "
    <> show name <> " "
    <> show args <> ")"
  show (Delay time) = "(Delay " <> show time <> ")"
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

data BotScript = BotScript (List Action) (Array BotState)
instance showBotScript :: Show BotScript where
  show (BotScript actions states) =
    "(BotScript " <> show actions <> " " <> show states <> ")"
