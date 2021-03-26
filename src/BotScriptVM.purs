module BotScriptVM where

import BotScript

import BotScriptEnv (Env(..), pushEnv, popEnv, assocVar, topBase
                    , changeBase, insert, update)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (unzip, zip, zipWith)
import Data.Array as A
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (find, foldr, traverse)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Prelude (Unit, bind, discard, map, pure, show, ($), (<$>), (<<<), (<>), (==))
import Undefined (undefined)

-- write state checker
foreign import none :: forall a. a -> Term
foreign import bool :: forall a. Term -> a -> a -> a

-- foreign import setTimer :: forall a. String -> Term -> Term -> Effect Unit
-- foreign import clearTimer :: String -> Effect Unit
-- foreign import clearAllTimer :: Effect Unit

foreign import newObject :: forall a. a -> Term

foreign import meetEvent ::
  Term -> String -> (Array String) -> Array Term -> Term -> Effect Unit
foreign import dropEvent :: Term -> String -> Effect Unit

foreign import dropTimer :: Term -> String -> Effect Unit
foreign import hangTimer :: Term -> String -> Term -> Term -> Effect Unit

foreign import toNumber :: Term -> Number
foreign import toBoolean :: Term -> Boolean
foreign import stringify :: forall a. a -> String

foreign import evalBin :: String -> Term -> Term -> Term
foreign import evalUna :: String -> Term -> Term
foreign import evalApp :: forall a. String -> Term -> a -> (Array Term) -> Term
foreign import memberOf :: Term -> Term -> Term
foreign import updMem :: forall a. Term -> a -> Term -> Effect Unit
foreign import toVaArgFunction :: forall a. a -> Term
foreign import new :: Term -> (Array Term) -> Term
foreign import delete :: Term -> Term -> Term

type MachineState = { val :: Term
                    , cur :: String
                    , env :: Env
                    , exprs :: List (List Expr)
                    , states :: Array BotState
                    , events :: Term
                    , timers :: Term
                    }

foreign import setMachine ::
  MachineState -> (Array String) -> (Array Term) -> MachineState

setExprs :: MachineState -> List (List Expr) -> MachineState
setExprs machine exprs = setMachine machine ["exprs"] [cast exprs]

setValExprs :: MachineState -> Term -> List (List Expr) -> MachineState
setValExprs machine val exprs = setMachine machine ["val", "exprs"] [val, cast exprs]

rawMachine :: forall a. a -> MachineState
rawMachine x = { val: none undefined
               , cur: ""
               , env: pushEnv Top
               , exprs: (Nil : Nil)
               , states: []
               , events: newObject undefined
               , timers: newObject undefined
               }

wrapMachine :: MachineState -> MachineState
wrapMachine machine = let
  _ = insert machine.env "__this__" (cast machine)
  env' = pushEnv machine.env
  _ = insert env' "__main__" (cast machine) in
  setMachine machine ["env"] [cast env']

cloneMachine :: MachineState -> MachineState
cloneMachine parent = let
  machine = rawMachine undefined
  _ = insert machine.env "__this__" (cast machine)
  env' = changeBase parent.env machine.env in
  setMachine machine
    ["val", "cur", "env", "exprs", "states", "events", "timers"]
    [cast parent.val, cast parent.cur, cast env'
    , cast parent.exprs, cast parent.states
    , cast parent.events, cast parent.timers]

evalExpr :: MachineState -> Expr -> Effect Term
evalExpr machine expr = do
    x <- tailRecM run $ setExprs machine ((expr : Nil): Nil)
    pure x.val


runExpr :: Expr -> MachineState -> Effect MachineState
runExpr expr machine = tailRecM run machine'
    where machine' = setExprs machine ((expr : Nil) : Nil)

lvalUpdate :: MachineState -> Expr -> Term -> Effect Env
lvalUpdate ms@{env: env} lval val =
    case lval of
        (Var name) ->
            pure $ insert env name val
        (Dot obj mem) -> do
            obj' <- evalExpr ms obj
            liftEffect $ updMem obj' mem val
            pure env
        (Sub obj sub) -> do
            obj' <- evalExpr ms obj
            sub' <- evalExpr ms sub
            liftEffect $ updMem obj' sub val
            pure env
        _ -> pure env

liftAbs :: Expr -> Expr
liftAbs abs@(Abs pars expr) = abs
liftAbs expr = Abs [] expr

bind'event'vars :: Array String -> Array String -> Env -> Env
bind'event'vars syms args enviorn =
  foldr (\(sym /\ arg) acc ->
    insert acc sym (cast arg)) enviorn (zip syms args)
    -- TODO: consider valueOf

make'event'action ::
    Array String ->
    Expr -> MachineState -> Term

make'event'action syms expr machine =
  let machine' = cloneMachine machine in
    toVaArgFunction (\args ->
    let env' = bind'event'vars (A.(:) "args" syms) args (pushEnv machine'.env)
        _ = setMachine machine' ["exprs", "env"] -- use cloneMachine here TODO
              [cast $ (expr : Nil) : Nil, cast env'] in do
        m <- tailRecM run machine'
        pure m.val
    )

evalExprLiftedStmt :: MachineState -> Expr -> Effect Term
evalExprLiftedStmt machine expr =
    evalExpr machine (case expr of
                     g@(Group _) -> liftAbs expr
                     _ -> expr)

detailShow :: Expr -> String
detailShow (Var name) = name
detailShow (Dot obj attr) = detailShow obj <> "." <> attr
detailShow (Sub obj attr) = detailShow obj <> "[" <> detailShow attr <> "]"
detailShow obj = show obj

unpackExprs :: List (List Expr) -> Tuple Expr (Tuple (List Expr) (List (List Expr)))
unpackExprs (Cons (Cons expr exprs) exprss) = (expr /\ exprs /\ exprss)
unpackExprs _ = ((Trm (toTerm "" "wrong")) /\ Nil /\ (Cons Nil Nil))

getEnv :: MachineState -> Env
getEnv machine = machine.env

run :: MachineState -> Effect (Step MachineState MachineState)

run machine@{ exprs: Nil } = do
   liftEffect $ log "Wrong Expr."
   pure (Done machine)

run machine@{ exprs: (Cons Nil Nil) } = do
   -- liftEffect $ log "Expr Done."
   pure (Done machine)

run machine@{ exprs: (Cons Nil rst) } =
    pure (Loop $ setMachine machine ["exprs", "env"] [cast rst, cast pop'env])
    where pop'env = popEnv machine.env

run machine@{ exprs: (Cons (Cons _ _) _) } =

    let (expr'cur /\ exprs /\ exprss) = unpackExprs machine.exprs
        -- triky skill to let side effect of machine state work
        exprs' = (exprs : exprss)
        env = getEnv machine in case expr'cur of

        (Trm term) -> do
           pure <<< Loop $ setValExprs machine term exprs'

        (Una "new" val) ->
            case val of
                 (App cons args) -> do
                    cons' <- evalExpr machine cons
                    args' <- traverse (evalExprLiftedStmt machine) args
                    pure <<< Loop $ setValExprs machine (new cons' args') exprs'
                 _ -> do
                    liftEffect <<< log $ "\"new\" need a constructor"
                    pure <<< Loop $ setValExprs machine (none undefined) exprs'

        (Una "delete" val) ->
            case val of
                (Dot obj mem) -> do
                   obj' <- evalExpr machine obj
                   (let mem' = (toTerm "String" mem) in
                        pure <<< Loop $ setValExprs machine (delete obj' mem') exprs')
                (Sub obj sub) -> do
                   obj' <- evalExpr machine obj
                   sub' <- evalExpr machine sub
                   pure <<< Loop $ setValExprs machine (delete obj' sub') exprs'
                _ -> do
                   expr' <- evalExpr machine val
                   pure <<< Loop $ setValExprs machine (delete expr' undefined) exprs'

        (Una op val) -> do
           val' <- evalExpr machine val
           (let val'' = evalUna op val'
                loop' = pure <<< Loop $ setValExprs machine val' exprs'
                loop'' = pure <<< Loop $ setValExprs machine val'' exprs' in
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
             then pure <<< Loop $ setValExprs machine lv' exprs'
             else do
                rv' <- evalExpr machine rv
                pure <<< Loop $ setValExprs machine rv' exprs'

        (Bin "&&" lv rv) -> do
           lv' <- evalExpr machine lv
           if toBoolean lv'
             then do
                rv' <- evalExpr machine rv
                pure <<< Loop $ setValExprs machine rv' exprs'
             else pure <<< Loop $ setValExprs machine lv' exprs'

        (Bin op lv rv) -> do
           lv' <- evalExpr machine lv
           rv' <- evalExpr machine rv
           pure <<< Loop $ setValExprs machine (evalBin op lv' rv') exprs'

        (Abs pars expr) ->
            let syms = map fst pars
                grds = map snd pars
                func = make'event'action syms expr machine
                -- func = expr'to'abs expr machine undefined
                                    in do
                -- guards <- traverse (evalExpr machine) grds
                -- ^ add guards?
                pure <<< Loop $ setValExprs machine func exprs'

        (App fn args) -> do
           -- TODO: split case
           args' <- traverse (evalExprLiftedStmt machine) args
           case fn of
                (Dot obj mem) -> do
                   obj' <- evalExpr machine obj
                   (let mem' = (toTerm "String" mem) in
                        pure <<< Loop $ setValExprs
                                          machine
                                          (evalApp (detailShow obj) obj' mem' args')
                                          exprs')
                (Sub obj sub) -> do
                   obj' <- evalExpr machine obj
                   sub' <- evalExpr machine sub
                   pure <<< Loop $ setValExprs
                                    machine
                                    (evalApp (detailShow obj) obj' sub' args')
                                    exprs'
                _ -> do
                   expr' <- evalExpr machine fn
                   pure <<< Loop $ setValExprs
                                    machine
                                    (evalApp (detailShow fn) expr' undefined args')
                                    exprs'

        (Sub obj sub) -> do
            obj' <- evalExpr machine obj
            sub' <- evalExpr machine sub
            pure <<< Loop $ setValExprs machine (memberOf obj' sub') exprs'

        (Arr array) -> do
           val <- toTerm "Array" <$> traverse (evalExpr machine) array
           pure <<< Loop $ setValExprs machine val exprs'

        (Dot obj mem) -> do
            obj' <- evalExpr machine obj
            (let mem' = toTerm "String" mem in
                 pure <<< Loop $ setValExprs machine (memberOf obj' mem') exprs')

        (Var name) ->
            -- need handle index
            case assocVar name env of
                Just term -> pure <<< Loop $ setValExprs machine term exprs'
                Nothing ->
                    let none' = none undefined
                        _ = insert env name none'
                     in pure <<< Loop $ setValExprs machine none' exprs'

        (Obj pairs) ->
            let keys /\ values = unzip pairs
                keys' = map (cast) keys in do
                values' <- traverse (evalExpr machine) values
                val <- evalExpr machine
                    (App (Dot (Var "Object") "fromEntries")
                        [Trm $ cast (zipWith
                        (\a b -> cast [a, b])
                        keys' values')])
                pure <<< Loop $ setValExprs machine val exprs'

        {- statement expression -}

        (Going dest) ->
            case find (\(BotState name _)
                       -> name == dest) machine.states of
              Just (BotState _ acts') ->
                  let base'env = topBase  env
                      _ = update base'env "__main__" (cast machine) in do
                      -- liftEffect $ setcur dest -- remove
                      liftEffect $ dropEvent machine.events machine.cur
                      -- liftEffect $ clearTimer machine.cur -- remove
                      liftEffect $ dropTimer machine.timers machine.cur
                      -- will not return, so clear env (static scoping)
                      pure (Loop $ setMachine machine
                           ["cur", "env", "exprs"]
                           [cast dest, cast base'env, cast $ (acts' : Nil) : Nil])
              Nothing -> do
                  liftEffect <<< log $
                      "state <" <> dest <> "> not found"
                  pure (Done machine)

        (Visit stat) ->
            case find (\(BotState name _)
                       -> name == stat) machine.states of
              Just (BotState _ acts') -> do
                      -- liftEffect $ setcur stat -- remove
                      liftEffect $ dropEvent machine.events machine.cur
                      -- liftEffect $ clearTimer machine.cur -- remove
                      liftEffect $ dropTimer machine.timers machine.cur
                      -- because will return , so no clear env (dynamic scoping)
                      pure (Loop $ setExprs machine
                           ((acts' : (Reset machine.cur) : exprs) : exprss))
              Nothing -> do
                  liftEffect <<< log $
                      "state <" <> stat <> "> not found"
                  pure (Done machine)

        (Reset stat) -> do
            -- liftEffect $ setcur stat -- remove
            liftEffect $ dropEvent machine.events machine.cur
            pure $ Loop (setExprs machine exprs')

        (Group actions) ->
            let new'env = (pushEnv env) in do
            pure (Loop $ setMachine machine
                    ["val", "env", "exprs"]
                    [ none undefined
                    , cast new'env
                    , cast (actions : exprs : exprss)
                    ])

        event@(Event etypes expr) ->
            let (pars /\ expr') = (case expr of
                       (Abs pars expr) -> pars /\ expr
                       _ -> [] /\ expr)
                syms = map fst pars
                grds = map snd pars in do
                guards <- traverse (evalExpr machine) grds
                -- liftEffect $ logShow event
                -- liftEffect $ listen machine.cur etypes guards -- remove
                liftEffect $ meetEvent machine.events machine.cur etypes guards -- remove
                    (make'event'action syms expr' machine)
                pure <<< Loop $ setValExprs machine (none undefined) exprs'

        (Renew lval val) -> do
           case lval of
                (Var name) -> do
                   val' <- evalExprLiftedStmt machine val
                   (let _ = insert env name val' in do
                       pure <<< Loop $ setValExprs machine val' exprs')
                (Dot obj mem) -> do
                   val' <- evalExprLiftedStmt machine val
                   obj' <- evalExpr machine obj
                   liftEffect $ updMem obj' mem val'
                   pure <<< Loop $ setValExprs machine val' exprs'
                (Sub obj sub) -> do
                   val' <- evalExprLiftedStmt machine val
                   obj' <- evalExpr machine obj
                   sub' <- evalExpr machine sub
                   liftEffect $ updMem obj' sub' val'
                   pure <<< Loop $ setValExprs machine val' exprs'
                _ -> do
                   val' <- evalExprLiftedStmt machine val
                   liftEffect $ logShow "invalid renew"
                   pure <<< Loop $ setValExprs machine val' exprs'

        (Ifels prd thn els) -> do
            prd' <- evalExpr machine prd
            (let act' = bool prd' thn els in
                pure (Loop $ setExprs machine ((act' : exprs) : exprss)))

        while@(While prd act) -> do
           prd' <- evalExpr machine prd
           (let nxt' = bool prd' (act : while : exprs) exprs in do
                pure (Loop $ setMachine machine
                     ["val", "exprs"]
                     [prd', cast (nxt' : exprss)]))

        (Timer prd expr) -> do
           expr' <- evalExpr machine $ liftAbs expr
           prd' <- evalExpr machine prd
           -- liftEffect $ setTimer machine.cur prd' expr' -- remove
           liftEffect $ hangTimer machine.timers machine.cur prd' expr'
           pure <<< Loop $ setValExprs machine (none undefined) exprs'

        (Later prd expr) -> do
           val <- evalExpr machine $ (App (Var "setTimeout") [liftAbs expr, prd])
           pure <<< Loop $ setValExprs machine val exprs'

        _ -> do
           liftEffect $ log $ "unhandled expression: " <> show expr'cur
           pure <<< Loop $ setValExprs machine (none undefined) exprs'

runVM :: BotScript -> Effect MachineState
runVM (BotScript exprs states) =
    -- clearAllTimer -- consider how to remove
    -- clearAllEvent -- remove
    let m = rawMachine undefined in do
    tailRecM run (wrapMachine $ setMachine m ["exprs", "states"] [cast (exprs : Nil), cast states])

runStep :: MachineState -> BotScript -> Effect MachineState
runStep machine (BotScript exprs states) = do
    tailRecM run (setValExprs machine (none undefined) (exprs : Nil))
