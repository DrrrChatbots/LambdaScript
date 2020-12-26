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
import Foreign.NullOrUndefined (null)
import Foreign.Object as FO
import Undefined (undefined)

-- write state checker
foreign import none :: forall a. a -> Term
foreign import print :: forall a. a -> Effect Unit

runVM (BotScript actions states) =
    let env = Env.pushEnv Env.Top in
    -- for_ actions (\a -> runAction a states env)
    void <<< Aff.launchAff $
        tailRecM runActions { as: (actions : Nil)
                            , e: env
                            , sn: ""
                            , s:states}

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

runActions { as: Nil
           , e: env
           , sn: sname
           , s: states} = do
   liftEffect $ log "wrong"
   pure (Done unit)

runActions { as: (Cons Nil Nil)
           , e: env
           , sn: sname
           , s: states} = do
   liftEffect $ log "done"
   pure (Done unit)

runActions { as: (Cons Nil rst)
           , e: env
           , sn: sname
           , s: states} =
    let pop'env = Env.popEnv env in
        pure (Loop { as: rst
                   , e: pop'env
                   , sn: sname, s: states})

runActions { as: (Cons (Cons a as) ra)
           , e: env
           , sn: sname
           , s: states} =
    case a of
        (Going dest) ->
            case find (\(BotState name _) -> name == dest) states of
              Just (BotState _ acts') ->
                  let top'env = Env.topEnv env in
                      pure (Loop { as: ((acts' : Nil) : Nil)
                                 , e: top'env
                                 , sn: dest
                                 , s: states})
              Nothing -> do
                  liftEffect <<< log $
                      "state <" <> dest <> "> not found"
                  pure (Done unit)

        (Group actions) ->
            let _ = logShow a
                new'env = (Env.pushEnv env) in
            pure (Loop { as: (actions : as : ra)
                       , e: new'env
                       , sn: sname
                       , s: states})

        (Value expr) ->
            let val = evalExpr env expr in do
                liftEffect <<< print $ val
                pure (Loop { as: (as : ra)
                           , e: env
                           , sn: sname
                           , s: states})

        (Event etype rules action) -> do
            liftEffect $ logShow a
            liftEffect $ listen sname etype (map snd rules) (make'event'action rules action sname states env)
            pure (Loop { as: (as : ra)
                       , e: env
                       , sn: sname
                       , s: states})

        (Renew lval val) ->
            let val' = evalExpr env val in do
              case lval of
                   (Var name) ->
                       let _ = Env.insert env name val' in
                        liftEffect $ logShow a
                   (Dot obj mem) ->
                       let obj' = evalExpr env obj in
                           liftEffect $ updMem obj' mem val'
                   (Sub obj sub) ->
                       let obj' = evalExpr env obj
                           sub' = evalExpr env sub in
                           liftEffect $ updMem obj' sub val'
                   _ -> liftEffect $ logShow "invalid renew"
              pure (Loop { as: (as : ra)
                         , e: env
                         , sn: sname
                         , s: states})

        (Value expr) ->
            let val = evalExpr env expr in do
                liftEffect $ log (val.toString undefined)
                pure (Loop { as: (as : ra)
                           , e: env
                           , sn: sname
                           , s: states})

        action -> do
            case action of
               -- builtins
               (Delay expr) -> do
                  liftEffect $ logShow a
                  Aff.delay (Milliseconds 1000.0)
               (While expr action) ->
                   liftEffect $ logShow a
               (Visit var expr action) ->
                   liftEffect $ logShow a
               (Match expr thn els) ->
                   liftEffect $ logShow a
               _ -> liftEffect $
                   log $ "unhandled action: " <> show a

            pure (Loop { as: (as : ra)
                       , e: env
                       , sn: sname
                       , s: states})

foreign import evalBin :: String -> Term -> Term -> Term
foreign import evalUna :: String -> Term -> Term
foreign import evalFun :: forall a. Term -> a -> (Array Term) -> Term
foreign import memberOf :: Term -> Term -> Term
foreign import updMem :: forall a. Term -> a -> Term -> Effect Unit


evalExpr :: Env.Env -> Expr -> Term
evalExpr env (Arr array) =
    toTerm "Array" $ map (evalExpr env) array

evalExpr env (Bin op lv rv) =
    evalBin op elv erv where
          elv = evalExpr env lv
          erv = evalExpr env rv

evalExpr env (Una op val) =
    evalUna op ev where
          ev = evalExpr env val

evalExpr env (Fun expr args) =
    -- TODO: split case
    case expr of
        (Dot obj mem) ->
            let obj' = evalExpr env obj
                mem' = toTerm "String" mem in
                evalFun obj' mem' args'
        (Sub obj sub) ->
            let obj' = evalExpr env obj
                sub' = evalExpr env sub in
                evalFun obj' sub' args'
        _ ->
            let expr' = evalExpr env expr in
                evalFun expr' undefined args'
    where args' = map (evalExpr env) args

evalExpr env (Sub obj sub) = memberOf obj' sub'
    where obj' = evalExpr env obj
          sub' = evalExpr env sub


evalExpr env (Dot obj mem) = memberOf obj' mem'
    where obj' = evalExpr env obj
          mem' = toTerm "String" mem

evalExpr env (Var name) =
    -- need handle index
    case Env.assocVar name env of
        Just term -> term
        Nothing ->
            let none' = none undefined
                _ = Env.insert env name none'
             in none'

evalExpr env (Trm term) = term

bind'event'vars :: (Array String) -> (Array (String /\ String)) -> Env.Env -> Env.Env
bind'event'vars args rules enviorn =
  foldr (\((name /\ _) /\ arg) acc ->
    Env.insert acc name (toTerm "String" arg)) enviorn (zip rules args)
    -- TODO: consider valueOf

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
