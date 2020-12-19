module BotScriptEnv where

import BotScript
import Data.Boolean
import Data.Maybe
import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Foreign.Object as FO

type Tab = FO.Object Expr
data Env = Env {lv:: Int, tab :: Tab, root :: Env} | Top

instance showEnv :: Show Env where
  show Top = "Top"
  show (Env e)
    = show e.root <> "\n"
    <> show e.lv <> " "
    <> show e.tab

pushEnv Top = Env {lv: 0, tab: FO.fromFoldable [], root: Top}
pushEnv env@(Env e)
    = Env {lv: e.lv + 1, tab: FO.fromFoldable [], root: env}

topEnv Top = Top
topEnv env@(Env e) =
    case e.root of
         Top -> env
         _ -> topEnv e.root

popEnv Top = Top
popEnv env@(Env e) = e.root

assocEnv key Top = Nothing
assocEnv key (Env e) =
    case FO.lookup key e.tab of
      Just _ -> Just e.tab
      Nothing -> assocEnv key e.root

assocVar name Top = Nothing
assocVar name (Env e) =
    case FO.lookup name e.tab of
      Just val -> Just val
      Nothing -> assocVar name e.root

foreign import updateTab :: String -> Expr -> Tab -> Boolean

{- update existed value only -}
update _ _ Top = false
update key val env =
    case assocEnv key env of
        Just tab -> updateTab key val tab
        Nothing -> false

{- insert/update a new value only -}
insert :: String -> Expr -> Env -> Env
insert key val Top = Top
insert key val env@(Env e) = let
    a = updateTab key val e.tab in env
