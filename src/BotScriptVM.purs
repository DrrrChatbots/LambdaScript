module BotScriptVM where

import BotScript
import Control.Lazy
import Data.Array
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

import Data.Array as A
import Data.List (List(..), (:))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
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
foreign import toBoolean :: Term -> Boolean
foreign import stringify :: forall a. a -> String

runExpr expr machine = tailRecM run machine'
    where machine' = machine { exprs = ((expr : Nil) : Nil) }

foreign import evalBin :: String -> Term -> Term -> Term
foreign import evalUna :: String -> Term -> Term
foreign import evalApp :: forall a. String -> Term -> a -> (Array Term) -> Term
foreign import memberOf :: Term -> Term -> Term
foreign import updMem :: forall a. Term -> a -> Term -> Effect Unit
foreign import toVaArgFunction :: forall a. a -> Term
foreign import new :: Term -> (Array Term) -> Term

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
    let env' = bind'event'vars (A.(:) "args" syms) args (Env.pushEnv env)
        machine' = machine { exprs = ((expr : Nil) : Nil)
                             , env = env'
                             } in do
        machine'' <- tailRecM run machine'
        pure machine''.val
    )

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

detailShow (Var name) = name
detailShow (Dot obj attr) = detailShow obj <> "." <> attr
detailShow (Sub obj attr) = detailShow obj <> "[" <> detailShow attr <> "]"
detailShow obj = show obj

run :: MachineState -> Effect (Step MachineState MachineState)

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


        (Una "new" val) ->
            case val of
                 (App cons args) -> do
                    cons' <- evalExpr machine cons
                    args' <- traverse (evalExprLiftedStmt machine) args
                    pure <<< Loop $ machine' { val = new cons' args' }
                 _ -> do
                    liftEffect <<< log $ "\"new\" need a constructor"
                    pure <<< Loop $ machine' { val = none undefined }

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

        (Bin "||" lv rv) -> do
           lv' <- evalExpr machine lv
           if toBoolean lv'
             then pure <<< Loop $ machine' { val = lv' }
             else do
                rv' <- evalExpr machine rv
                pure <<< Loop $ machine' { val = rv' }

        (Bin "&&" lv rv) -> do
           lv' <- evalExpr machine lv
           if toBoolean lv'
             then do
                rv' <- evalExpr machine rv
                pure <<< Loop $ machine' { val = rv' }
             else pure <<< Loop $ machine' { val = lv' }

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
                        pure <<< Loop $ machine' { val = evalApp (detailShow obj) obj' mem' args' })
                (Sub obj sub) -> do
                   obj' <- evalExpr machine obj
                   sub' <- evalExpr machine sub
                   pure <<< Loop $ machine' { val = evalApp (detailShow obj) obj' sub' args' }
                _ -> do
                   expr' <- evalExpr machine fn
                   pure <<< Loop $ machine' { val = evalApp (detailShow fn) expr' undefined args' }

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

        (Obj pairs) ->
            let keys /\ values = unzip pairs
                keys' = map (toTerm "") keys in do
                values' <- traverse (evalExpr machine) values
                val <- evalExpr machine
                    (App (Dot (Var "Object") "fromEntries")
                        [Trm $ toTerm "" (zipWith
                        (\a b -> toTerm "" [a, b])
                        keys' values')])
                pure <<< Loop $ machine' { val = val }

        {- statement expression -}

        (Going dest) ->
            case find (\(BotState name _)
                       -> name == dest) machine.states of
              Just (BotState _ acts') ->
                  let top'env = Env.topEnv env in do
                      liftEffect $ setcur dest
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
                      liftEffect $ setcur stat
                      liftEffect $ clearTimer machine.cur
                      pure (Loop $ machine { cur = stat
                                         , env = top'env
                                         , exprs = ((acts' : (Reset machine.cur) : exprs) : exprss)
                                         })
              Nothing -> do
                  liftEffect <<< log $
                      "state <" <> stat <> "> not found"
                  pure (Done machine)

        (Reset stat) -> do
            liftEffect $ setcur stat
            pure $ Loop machine'

        (Group actions) ->
            let new'env = (Env.pushEnv env) in
            pure (Loop $ machine { env = new'env
                                 , val = none undefined
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
                -- liftEffect $ logShow event
                liftEffect $ listen machine.cur etypes guards
                    (make'event'action syms expr' machine)
                pure <<< Loop $ machine' { val = none undefined }

        (Renew lval val) -> do
           val' <- evalExprLiftedStmt machine val
           case lval of
                (Var name) ->
                   (let _ = Env.insert env name val' in do
                       pure <<< Loop $ machine' {val = val'})
                (Dot obj mem) -> do
                   obj' <- evalExpr machine obj
                   liftEffect $ updMem obj' mem val'
                   pure <<< Loop $ machine' {val = val'}
                (Sub obj sub) -> do
                   obj' <- evalExpr machine obj
                   sub' <- evalExpr machine sub
                   liftEffect $ updMem obj' sub' val'
                   pure <<< Loop $ machine' {val = val'}
                _ -> do
                   liftEffect $ logShow "invalid renew"
                   pure <<< Loop $ machine' {val = val'}

        (Ifels prd thn els) -> do
            prd' <- evalExpr machine prd
            (let act' = bool prd' thn els in
                pure (Loop $ machine { exprs = ((act' : exprs) : exprss) }))

        while@(While prd act) -> do
           prd' <- evalExpr machine prd
           (let nxt' = bool prd' (act : while : exprs) exprs in do
                pure (Loop $ machine { val = prd'
                                     , exprs = (nxt' : exprss) }))

        (Timer prd expr) -> do
           expr' <- evalExpr machine $ liftAbs expr
           prd' <- evalExpr machine prd
           liftEffect $ setTimer machine.cur prd' expr'
           pure <<< Loop $ machine' { val = none undefined }

        (Later prd expr) -> do
           val <- evalExpr machine $ (App (Var "setTimeout") [liftAbs expr, prd])
           pure <<< Loop $ machine' { val = val }

        action -> do
            case action of
               -- builtins
               _ -> liftEffect $
                   log $ "unhandled expression: " <> show expr'cur

            pure <<< Loop $ machine' { val = none undefined }


runVM (BotScript exprs states) = do
    clearAllTimer
    clearAllEvent
    tailRecM run { val: none undefined
                 , cur: ""
                 , env: Env.pushEnv Env.Top
                 , exprs: (exprs : Nil)
                 , states: states
                 }

runStep machine (BotScript exprs states) = do
    tailRecM run (machine { val = none undefined
                          , exprs = (exprs : Nil)
                          })
