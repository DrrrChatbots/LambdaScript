module BotScriptEnv where

import BotScript (Term)
import Data.Maybe (Maybe(..))
import Prelude (class Show, Unit, show, (+), (<>))

import Effect (Effect)
import Foreign.Object as FO
import Undefined (undefined)

type Tab = FO.Object Term

foreign import global :: Tab

showTab :: Tab -> String
showTab tab = "{\n" <> FO.fold (\acc key val -> acc <> ", " <> key <> ": " <> val.toString undefined <> "\n") "" tab <> "}"

data Env = Env {lv:: Int, tab :: Tab, root :: Env} | Top

instance showEnv :: Show Env where
  show Top = "Top"
  show (Env e)
    = show e.root <> "\n"
    <> show e.lv <> " "
    <> showTab e.tab

pushEnv :: Env -> Env
pushEnv Top =
  Env { lv: 0
  , tab: FO.fromFoldable []
  , root: Top
  }

pushEnv env@(Env e) =
  Env { lv: e.lv + 1
  , tab: FO.fromFoldable []
  , root: env
  }

topEnv :: Env -> Env
topEnv Top = Top
topEnv env@(Env e) =
    case e.root of
         Top -> env
         _ -> topEnv e.root

topBase :: Env -> Env
topBase Top = Top
topBase srcEnv@(Env e) =
    case e.root of
       Top -> Top
       rootEnv@(Env env) -> (
        case env.root of
             Top -> srcEnv
             _ -> topBase rootEnv
       )

popEnv :: Env -> Env
popEnv Top = Top
popEnv env@(Env e) = e.root

assocEnv :: String -> Env -> Maybe Tab
assocEnv key Top = Nothing
assocEnv key (Env e) =
    case FO.lookup key e.tab of
      Just _ -> Just e.tab
      Nothing -> assocEnv key e.root

assocVar :: String -> Env -> Maybe Term
assocVar name Top =
    case FO.lookup name global of
      Just val -> Just val
      Nothing -> Nothing

assocVar name (Env e) =
    case FO.lookup name e.tab of
      Just val -> Just val
      Nothing -> assocVar name e.root

changeBase :: Env -> Env -> Env
changeBase Top baseEnv = Top
changeBase (Env srcEnv) baseEnv =
  case srcEnv.root of
       Top -> baseEnv
       rootEnv ->
         Env { lv: srcEnv.lv
         , tab: srcEnv.tab
         , root: changeBase rootEnv baseEnv
         }

{-
  cur -> global -> base -> Top
               .root  .root   == Top


-}

foreign import updateTab :: Tab -> String -> Term -> Boolean
foreign import updateTabRef :: Tab -> String -> (Array Term) -> Term -> Effect Unit

{- update existed value only -}
update :: Env -> String -> Term -> Boolean
update Top _ _ = false
update env key val =
    case assocEnv key env of
        Just tab -> updateTab tab key val
        Nothing -> false

{- insert/update a new value only -}
insert :: Env -> String -> Term -> Env
insert Top key val = Top
insert env@(Env e) key val = let
    exist = update env key val
    _ = if exist
        then true
        else updateTab e.tab key val in
    env
