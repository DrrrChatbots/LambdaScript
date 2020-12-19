module BotScriptVM where

import BotScript
import BotScriptEnv
import Control.Lazy
import Data.Array
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Tuple.Nested
import DrrrInvok
import Prelude

import Control.Comonad.Env (env)
import Control.Monad.Rec.Class (Step(..), tailRec, tailRecM, tailRecM2, untilJust, whileJust)
import Data.List (List(..), (:))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Foreign.Object as FO

-- write state checker

runVM (BotScript actions states) =
    let env = pushEnv Top in
    -- for_ actions (\a -> runAction a states env)
    void <<< Aff.launchAff $
        tailRecM runActions { as: (actions : Nil), e: env, s:states}

runActions {as: Nil, e: env, s: states} = do
   liftEffect $ log "wrong"
   pure (Done unit)

runActions {as: (Cons Nil Nil), e: env, s: states} = do
   liftEffect $ log "done"
   pure (Done unit)

runActions {as: (Cons Nil rst), e: env, s: states} =
    let pop'env = popEnv env in
        pure (Loop {as: rst ,e: pop'env, s: states})

runActions {as: (Cons (Cons a as) ra), e: env, s: states} =
    case a of
        (Going dest) ->
            case find (\(BotState name _) -> name == dest) states of
              Just (BotState _ new'actions) ->
                  let top'env = topEnv env in
                      pure (Loop {as: ((new'actions : Nil) : Nil), e: top'env, s: states})
              Nothing -> do
                  liftEffect <<< log $ "state <" <> dest <> "> not found"
                  pure (Done unit)

        (Group actions) ->
            let _ = logShow a
                new'env = (pushEnv env) in
            pure (Loop {as: (actions : as : ra), e: new'env, s: states})

        (Invok name args) -> do
            -- liftEffect $ logShow a
            liftEffect $ invok name args
            pure (Loop {as: (as : ra), e: env, s: states})

        action -> do
            case action of
               -- builtins
               (Delay expr) -> do
                  liftEffect $ logShow a
                  Aff.delay (Milliseconds 1000.0)
               (Renew var expr) -> liftEffect $ logShow a
               (While expr action) -> liftEffect $ logShow a
               (Visit var expr action) -> liftEffect $ logShow a
               (Match expr thn els) -> liftEffect $ logShow a
               (Event etype rules action) -> liftEffect $ logShow a
               _ -> liftEffect $ log $ "unhandled action: " <> show a

            pure (Loop {as: (as : ra), e: env, s: states})

evalExpr env (Arr array) = Arr $ map (evalExpr env) array
evalExpr env (Bin op lv rv) =
        case op of
             "/" -> Null
             "*" -> Null
             "-" -> Null
             "+" -> Null
             "&" -> Null
             "|" -> Null
             _ -> Null

evalExpr env (Una op val) =
    case op of
        "!" -> Null
        "-" -> Null
        _ -> Null

evalExpr env (Ref (Var name idxs)) =
    -- need handle index
    case assocVar name env of
        Just expr -> expr
        Nothing -> Null

evalExpr env expr = expr
