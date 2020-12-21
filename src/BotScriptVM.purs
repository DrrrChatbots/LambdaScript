module BotScriptVM where

import BotScript
import Control.Lazy
import Data.Array
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import DrrrBot
import Prelude

import BotScriptEnv as Env
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
    let env = Env.pushEnv Env.Top in
    -- for_ actions (\a -> runAction a states env)
    void <<< Aff.launchAff $
        tailRecM runActions { as: (actions : Nil), e: env, sn: "", s:states}

runActions :: { as :: List (List Action)
                  , e :: Env.Env
                  , sn :: String
                  , s :: Array BotState
                  } -> Aff.Aff (Step
                        { as :: List (List Action)
                        , e :: Env.Env
                        , sn :: String
                        , s :: Array BotState
                        }
                        Unit
                       )

runActions {as: Nil, e: env, sn: sname, s: states} = do
   liftEffect $ log "wrong"
   pure (Done unit)

runActions {as: (Cons Nil Nil), e: env, sn: sname, s: states} = do
   liftEffect $ log "done"
   pure (Done unit)

runActions {as: (Cons Nil rst), e: env, sn: sname, s: states} =
    let pop'env = Env.popEnv env in
        pure (Loop {as: rst ,e: pop'env, sn: sname, s: states})

runActions {as: (Cons (Cons a as) ra), e: env, sn: sname, s: states} =
    case a of
        (Going dest) ->
            case find (\(BotState name _) -> name == dest) states of
              Just (BotState _ new'actions) ->
                  let top'env = Env.topEnv env in
                      pure (Loop {as: ((new'actions : Nil) : Nil), e: top'env, sn: dest, s: states})
              Nothing -> do
                  liftEffect <<< log $ "state <" <> dest <> "> not found"
                  pure (Done unit)

        (Group actions) ->
            let _ = logShow a
                new'env = (Env.pushEnv env) in
            pure (Loop {as: (actions : as : ra), e: new'env, sn: sname, s: states})

        (Invok name args) ->
            let ev'args = map (evalExpr env) args in do
                liftEffect $ invok name ev'args
                pure (Loop {as: (as : ra), e: env, sn: sname, s: states})

        (Event etype rules action) -> do
            liftEffect $ logShow a
            liftEffect $ listen sname etype (map snd rules) (make'event'action rules action sname states env)
            pure (Loop {as: (as : ra), e: env, sn: sname, s: states})

        (Renew (Var name []) expr) ->
           let ev'expr = evalExpr env expr
               nenv = Env.insert env name ev'expr in do
               liftEffect $ logShow a
               pure (Loop {as: (as : ra), e: env, sn: sname, s: states})

        (Renew (Var name idxs) expr) ->
           let ev'expr = evalExpr env expr
               ev'idxs = map (evalExpr env) idxs in do
            case Env.assocEnv name env of
                Nothing -> liftEffect $
                     log (show a <> " var not found")
                Just tab -> liftEffect $
                     Env.updateTabRef tab name ev'idxs ev'expr
            pure (Loop {as: (as : ra), e: env, sn: sname, s: states})

        action -> do
            case action of
               -- builtins
               (Delay expr) -> do
                  liftEffect $ logShow a
                  Aff.delay (Milliseconds 1000.0)
               (While expr action) -> liftEffect $ logShow a
               (Visit var expr action) -> liftEffect $ logShow a
               (Match expr thn els) -> liftEffect $ logShow a
               _ -> liftEffect $ log $ "unhandled action: " <> show a

            pure (Loop {as: (as : ra), e: env, sn: sname, s: states})

foreign import evalBin :: forall a. (a -> Expr) -> String -> Expr -> Expr -> Expr
foreign import evalUna :: forall a. (a -> Expr) -> String -> Expr -> Expr
foreign import evalRef :: forall a. (a -> Expr) -> Expr -> (Array Expr) -> Expr

evalExpr env (Arr array) = Arr $ map (evalExpr env) array
evalExpr env (Bin op lv rv) =
    evalBin toExpr op elv erv where
          elv = evalExpr env lv
          erv = evalExpr env rv

evalExpr env (Una op val) =
    evalUna toExpr op ev where
          ev = evalExpr env val

evalExpr env (Ref (Var name idxs)) =
    -- need handle index
    case Env.assocVar name env of
        Just expr -> expr
        Nothing -> Null
        where ev'idxs = map (evalExpr env) idxs

evalExpr env expr = expr

bind'event'vars :: (Array String) -> (Array (String /\ String)) -> Env.Env -> Env.Env
bind'event'vars args rules enviorn =
  foldr (\((name /\ _) /\ arg) acc ->
    Env.insert acc name (Str arg)) enviorn (zip rules args)

make'event'action ::
    Array (Tuple String String) ->
    Action -> String -> (Array BotState) ->
    Env.Env -> Array String -> Effect Unit

make'event'action rules action sname states env
    = \args ->
        void <<< Aff.launchAff $
        tailRecM runActions
        { as: ((action: Nil) : Nil)
        , e: bind'event'vars args rules env
        , sn: sname
        , s: states
        }
