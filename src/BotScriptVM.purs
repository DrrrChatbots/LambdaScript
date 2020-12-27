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
foreign import bool :: forall a. Term -> a -> a -> a

runVM (BotScript actions states) =
    let env = Env.pushEnv Env.Top in
    -- for_ actions (\a -> runAction a states env)
    void <<< Aff.launchAff $
        tailRecM runActions { as: (actions : Nil)
                            , e: env
                            , sn: ""
                            , s: states}

runAction act env sname states =
        tailRecM runActions { as: ((act : Nil) : Nil)
                            , e: env
                            , sn: sname
                            , s: states}

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
   liftEffect $ log "Wrong Action."
   pure (Done unit)

runActions { as: (Cons Nil Nil)
           , e: env
           , sn: sname
           , s: states} = do
   liftEffect $ log "Action Done."
   pure (Done unit)

runActions { as: (Cons Nil rst)
           , e: env
           , sn: sname
           , s: states} =
    let pop'env = Env.popEnv env in
        pure (Loop { as: rst
                   , e: pop'env
                   , sn: sname
                   , s: states})

runActions { as: (Cons (Cons a as) ra)
           , e: env
           , sn: sname
           , s: states} =
    let next'loop = Loop { as: (as : ra)
                          , e: env
                          , sn: sname
                          , s: states} in
    case a of
        (Going dest) ->
            case find (\(BotState name _)
                       -> name == dest) states of
              Just (BotState _ acts') ->
                  let top'env = Env.topEnv env in do
                      liftEffect $ unlisten sname
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
                pure next'loop

        (Event etype rules action) -> do
            liftEffect $ logShow a
            liftEffect $ listen sname etype
                            (map snd rules)
                            (make'event'action rules action
                                        sname states env)
            pure next'loop

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
              pure next'loop

        (Value expr) ->
            let val = evalExpr env expr in do
                liftEffect $ log (val.toString undefined)
                pure next'loop

        (Ifels prd thn els) ->
            let prd' = evalExpr env prd
                act' = bool prd' thn els in do
                pure (Loop { as: ((act' : as) : ra)
                           , e: env
                           , sn: sname
                           , s: states})


        while@(While prd act) ->
           let prd' = evalExpr env prd
               nxt' = bool prd' (act : while : as) as in do
                pure (Loop { as: (nxt' : ra)
                           , e: env
                           , sn: sname
                           , s: states})

        action -> do
            case action of
               -- builtins
               (Delay expr) -> do
                  liftEffect $ logShow a
                  Aff.delay (Milliseconds 1000.0)
               (Visit var expr action) ->
                   liftEffect $ logShow a
               _ -> liftEffect $
                   log $ "unhandled action: " <> show a

            pure next'loop

foreign import evalBin :: String -> Term -> Term -> Term
foreign import evalUna :: String -> Term -> Term
foreign import evalFun :: forall a. Term -> a -> (Array Term) -> Term
foreign import memberOf :: Term -> Term -> Term
foreign import updMem :: forall a. Term -> a -> Term -> Effect Unit

lvalUpdate env lval val =
    case lval of
        (Var name) ->
            Env.insert env name val
        (Dot obj mem) ->
            let obj' = evalExpr env obj
                _ = updMem obj' mem val in env
        (Sub obj sub) ->
            let obj' = evalExpr env obj
                sub' = evalExpr env sub
                _ = updMem obj' sub val in env
        _ -> env



evalExpr :: Env.Env -> Expr -> Term
evalExpr env (Arr array) =
    toTerm "Array" $ map (evalExpr env) array

evalExpr env (Bin op lv rv) =
    evalBin op lv' rv' where
          lv' = evalExpr env lv
          rv' = evalExpr env rv

evalExpr env (Una op val) =
    case op of
         "_++" ->
             let _ = lvalUpdate env val val'' in val'
         "_--" ->
             let _ = lvalUpdate env val val'' in val'
         "++_" ->
             let _ = lvalUpdate env val val'' in val''
         "--_" ->
             let _ = lvalUpdate env val val'' in val''
         _ -> val''
    where val' = evalExpr env val
          val'' = evalUna op val'

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
