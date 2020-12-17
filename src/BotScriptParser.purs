module BotScriptParser where

import BotScript
import Control.Lazy
import Data.Array
import Data.Foldable
import Data.Functor
import Data.Int
import Data.Maybe
import Data.String
import Data.Tuple.Nested
import Prelude
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.String.CodeUnits (fromCharArray, singleton)
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.String (anyChar, char, eof, satisfy, string, whiteSpace)
import Text.Parsing.Parser.Token (makeTokenParser)

tokParser = makeTokenParser javaStyle
parens p = whiteSpace *> tokParser.parens p
braces p = whiteSpace *> tokParser.braces p
brackets p = whiteSpace *> tokParser.brackets p
parseIdentifier = (whiteSpace *> tokParser.identifier)
parseStringLiteral = (whiteSpace *> tokParser.stringLiteral)
reserved xs = try (whiteSpace *> string xs)

parseNumber = whiteSpace *>
    (try tokParser.float
    <|> try (tokParser.integer >>= toNumber >>> pure))

parseVar =
  parseIdentifier >>= \name ->
  many (brackets $ parseExpr) >>= \subs ->
  pure $ Var name subs

parseArray = fix $ \self ->
  (brackets $ parseExpr `sepBy` (string ",")
      >>= fromFoldable >>> Arr >>> pure)

event'types =
    [ ("join" /\ Join)
    , ("left" /\ Exit)
    , ("play" /\ Play)
    , ("over" /\ Over)
    , ("milk" /\ Milk)
    , ("talk" /\ Talk)
    , ("priv" /\ Priv)
    ]

parseEtype = choice $ map (\(n /\ c)->
    (reserved n *> pure c))
    event'types

parsePattern = do
    var <- parseIdentifier
    maybe <- optionMaybe (reserved ":" *> parseStringLiteral)
    case maybe of
         Just pattern -> pure $ var /\ pattern
         Nothing -> pure $ var /\ ""

parseRules = fix $ \self ->
  (parens $ parsePattern `sepBy` (reserved ",")
      >>= fromFoldable >>> pure)

-- try to prevent consume input
parseExpr = fix $ \self ->
  try (parseStringLiteral >>= Str >>> pure)
  <|> try (parseNumber >>= Num >>> pure)
  <|> try (parseVar >>= Ref >>> pure)
  <|> try parseArray
  -- UnaOp
  -- BinOp

expr'action =
    [ ("title" /\ Title)
    , ("descr" /\ Descr)
    , ("delay" /\ Delay)
    , ("print" /\ Print)
    , ("order" /\ Order)
    {- Going need use identifier -}
    -- , ("going" /\ Going)
    ]

parseBinding =
  try (parseVar >>= \var ->
      string "=" *>
      parseExpr >>= \expr ->
      pure $ Renew var expr)

parseAction = fix $ \self ->
  (choice $
    map (\(n /\ c) ->
        (reserved n *> parseExpr) >>= c >>> pure)
        expr'action)
  <|> (do
      _ <- reserved "event"
      name <- parseEtype
      rules <- parseRules
      action <- parseAction
      pure $ Event name rules action)
  <|> (braces $ many self >>= Group >>> pure)
  <|> (reserved "going" *> parseIdentifier >>= Going >>> pure)
  <|> parseBinding
  -- Timer
  -- while
  -- visit
  -- match

parseState = do
  name <- reserved "state" *>
          parseIdentifier
  action <- parseAction
  pure $ BotState name action

parseScript = do
    bindings <- (many parseBinding)
    states <- (many parseState)
    pure $ BotScript bindings states

parse p str = runParser str $ p

parseShow p ctx = case parse p ctx of
  Right actual ->
      log (show actual)
  Left err -> log ("error: " <> show err)
