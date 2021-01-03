module BotScriptVM where


import BotScript
import Control.Lazy
import Data.Array
import Data.Array as A
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Data.Tuple.Nested
import DrrrBot
import Prelude

import BotScriptEnv (Env(..))
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
foreign import bool :: forall a. Term -> a -> a -> a
foreign import setTimer :: forall a. String -> Term -> Term -> Effect Unit
foreign import clearTimer :: String -> Effect Unit
foreign import clearAllTimer :: Effect Unit
foreign import toNumber :: Term -> Number

runExpr expr machine = tailRecM run machine'
    where machine' = machine { exprs = ((expr : Nil) : Nil) }

foreign import evalBin :: String -> Term -> Term -> Term
foreign import evalUna :: String -> Term -> Term
foreign import evalApp :: forall a. Term -> a -> (Array Term) -> Term
foreign import memberOf :: Term -> Term -> Term
foreign import updMem :: forall a. Term -> a -> Term -> Effect Unit
foreign import toVaArgFunction :: forall a. a -> Term

lvalUpdate ms@{env: env} lval val =
    case lval of
        (Var name) ->
            pure $ Env.insert env name val
        (Dot obj mem) -> do
            obj' <- evalExpr ms obj
            (let _ = updMem obj' mem val in pure env)
        (Sub obj sub) -> do
            obj' <- evalExpr ms obj
            sub' <- evalExpr ms sub
            (let _ = updMem obj' sub val in pure env)
        _ -> pure env


liftAbs abs@(Abs pars expr) = abs
liftAbs expr = Abs [] expr

bind'event'vars :: Array String -> Array String -> Env.Env -> Env.Env
bind'event'vars syms args enviorn =
  foldr (\(sym /\ arg) acc ->
    Env.insert acc sym (toTerm "" arg)) enviorn (zip syms args)
    -- TODO: consider valueOf

make'event'action ::
    Array String ->
    Expr -> MachineState -> Term

make'event'action syms expr machine@{env: env} =
    toVaArgFunction (\args ->
    let env' = bind'event'vars (A.(:) "args" syms) args env
        machine' = machine { exprs = ((expr : Nil) : Nil)
                             , env = env'
                             } in
        void <<< Aff.launchAff $ tailRecM run machine')

type MachineState = { val :: Term
                    , cur :: String
                    , env :: Env.Env
                    , exprs :: List (List Expr)
                    , states :: Array BotState
                    }

fms = { val: toTerm "" ""
      , cur: "hello"
      , env: Env.Top
      , exprs: Nil
      , states : []
      }

-- evalExpr :: MachineState -> Expr -> Term
evalExpr machine expr = do
    x <- tailRecM run $ machine { exprs = ((expr : Nil) : Nil) }
    pure x.val

evalExprLiftedStmt machine expr =
    evalExpr machine (case expr of
                     g@(Group _) -> liftAbs expr
                     _ -> expr)


run :: MachineState -> Aff.Aff (Step MachineState MachineState)

run machine@{ exprs: Nil } = do
   liftEffect $ log "Wrong Expr."
   pure (Done machine)

run machine@{ exprs: (Cons Nil Nil) } = do
   -- liftEffect $ log "Expr Done."
   pure (Done machine)

run machine@{ exprs: (Cons Nil rst) } =
    pure (Loop $ machine { exprs = rst , env = pop'env})
    where pop'env = Env.popEnv machine.env

run machine@{ exprs: (Cons (Cons expr'cur exprs) exprss), env: env } =

    let machine' = machine { exprs = (exprs : exprss) } in case expr'cur of

        (Trm term) -> pure <<< Loop $ machine' { val = term }

        (Una op val) -> do
           val' <- evalExpr machine val
           (let val'' = evalUna op val'
                loop' = pure <<< Loop $ machine' { val = val' }
                loop'' = pure <<< Loop $ machine' { val = val'' } in
                case op of
                     "_++" -> do
                         _ <- lvalUpdate machine val val''
                         loop'
                     "_--" -> do
                         _ <- lvalUpdate machine val val''
                         loop'
                     "++_" -> do
                         _ <- lvalUpdate machine val val''
                         loop''
                     "--_" -> do
                         _ <- lvalUpdate machine val val''
                         loop''
                     _ -> loop'')

        (Bin op lv rv) -> do
           lv' <- evalExpr machine lv
           rv' <- evalExpr machine rv
           pure <<< Loop $ machine' { val = evalBin op lv' rv' }

        (Abs pars expr) ->
            let syms = map fst pars
                grds = map snd pars
                func = make'event'action syms expr machine
                -- func = expr'to'abs expr machine undefined
                                    in do
                -- guards <- traverse (evalExpr machine) grds
                -- ^ add guards?
                pure <<< Loop $ machine' { val = func }

        (App fn args) -> do
           -- TODO: split case
           args' <- traverse (evalExprLiftedStmt machine) args
           case fn of
                (Dot obj mem) -> do
                   obj' <- evalExpr machine obj
                   (let mem' = (toTerm "String" mem) in
                        pure <<< Loop $ machine' { val = evalApp obj' mem' args' })
                (Sub obj sub) -> do
                   obj' <- evalExpr machine obj
                   sub' <- evalExpr machine sub
                   pure <<< Loop $ machine' { val = evalApp obj' sub' args' }
                _ -> do
                   expr' <- evalExpr machine fn
                   pure <<< Loop $ machine' { val = evalApp expr' undefined args' }

        (Sub obj sub) -> do
            obj' <- evalExpr machine obj
            sub' <- evalExpr machine sub
            pure <<< Loop $ machine' { val = memberOf obj' sub' }

        (Arr array) -> do
           val <- toTerm "Array" <$> traverse (evalExpr machine) array
           pure <<< Loop $ machine' { val = val }

        (Dot obj mem) -> do
            obj' <- evalExpr machine obj
            (let mem' = toTerm "String" mem in
                 pure <<< Loop $ machine' { val = memberOf obj' mem' })

        (Var name) ->
            -- need handle index
            case Env.assocVar name machine.env of
                Just term -> pure <<< Loop $ machine' { val = term }
                Nothing ->
                    let none' = none undefined
                        _ = Env.insert machine.env name none'
                     in pure <<< Loop $ machine' { val = none' }


        {- statement expression -}

        (Going dest) ->
            case find (\(BotState name _)
                       -> name == dest) machine.states of
              Just (BotState _ acts') ->
                  let top'env = Env.topEnv env in do
                      liftEffect $ unlisten machine.cur
                      liftEffect $ clearTimer machine.cur
                      pure (Loop $ machine { cur = dest
                                           , env = top'env
                                           , exprs = ((acts' : Nil) : Nil)})
              Nothing -> do
                  liftEffect <<< log $
                      "state <" <> dest <> "> not found"
                  pure (Done machine)

        (Visit stat) ->
            case find (\(BotState name _)
                       -> name == stat) machine.states of
              Just (BotState _ acts') ->
                  let top'env = Env.topEnv env in do
                      liftEffect $ unlisten machine.cur
                      liftEffect $ clearTimer machine.cur
                      pure (Loop machine { cur = stat
                                         , env = top'env
                                         , exprs = ((acts' : exprs) : exprss)
                                         })
              Nothing -> do
                  liftEffect <<< log $
                      "state <" <> stat <> "> not found"
                  pure (Done machine)

        (Group actions) ->
            let new'env = (Env.pushEnv env) in
            pure (Loop $ machine { env = new'env
                                 , exprs = (actions : exprs : exprss)
                                 })

        -- TODO
        event@(Event etypes expr) ->
            let (pars /\ expr') = (case expr of
                       (Abs pars expr) -> pars /\ expr
                       _ -> [] /\ expr)
                syms = map fst pars
                grds = map snd pars in do
                guards <- traverse (evalExpr machine) grds
                liftEffect $ logShow event
                liftEffect $ listen machine.cur etypes guards
                    (make'event'action syms expr' machine)
                pure $ Loop machine'

        (Renew lval val) -> do
           val' <- evalExprLiftedStmt machine val
           case lval of
                (Var name) ->
                   (let _ = Env.insert env name val' in do
                        pure $ Loop machine')
                (Dot obj mem) -> do
                   obj' <- evalExpr machine obj
                   liftEffect $ updMem obj' mem val'
                   pure $ Loop machine'
                (Sub obj sub) -> do
                   obj' <- evalExpr machine obj
                   sub' <- evalExpr machine sub
                   liftEffect $ updMem obj' sub val'
                   pure $ Loop machine'
                _ -> do
                   liftEffect $ logShow "invalid renew"
                   pure $ Loop machine'

        (Ifels prd thn els) -> do
            prd' <- evalExpr machine prd
            (let act' = bool prd' thn els in
                pure (Loop $ machine { exprs = ((act' : exprs) : exprss) }))

        while@(While prd act) -> do
           prd' <- evalExpr machine prd
           (let nxt' = bool prd' (act : while : exprs) exprs in do
                pure (Loop $ machine { exprs = (nxt' : exprss) }))

        (Timer prd expr) -> do
           expr' <- evalExpr machine $ liftAbs expr
           prd' <- evalExpr machine prd
           liftEffect $ setTimer machine.cur prd' expr'
           pure $ Loop machine'

        (Delay expr) -> do
           expr' <- evalExpr machine expr
           (let period = toNumber expr' in
                Aff.delay (Milliseconds period))
           pure $ Loop machine'

        action -> do
            case action of
               -- builtins
               _ -> liftEffect $
                   log $ "unhandled expression: " <> show expr'cur

            pure $ Loop machine'


runVM (BotScript exprs states) =
    let env = Env.pushEnv Env.Top in do
    clearAllTimer
    clearAllEvent
    void <<< Aff.launchAff $
        tailRecM run { val: none undefined
                     , cur: ""
                     , env: env
                     , exprs: (exprs : Nil)
                     , states: states
                     }
