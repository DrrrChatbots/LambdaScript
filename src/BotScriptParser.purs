module BotScriptParser where

import BotScript
import Control.Lazy
import Data.Array
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Int
import Data.Maybe
import Data.String
import Data.Tuple.Nested
import Prelude
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List (List)
import Data.List as L
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

parseBoolean = whiteSpace *>
    (reserved "true" *> pure true)
    <|> (reserved "false" *> pure false)

parseNumber = whiteSpace *>
    (try tokParser.float
    <|> try (toNumber <$> tokParser.integer))

parseVar =
  parseIdentifier >>= \name ->
  many (brackets $ parseExpr) >>= \subs ->
  pure $ Var name subs

parseArray = fix $ \self ->
  (brackets $ fromFoldable >>> Arr <$>
      parseExpr `sepBy` (string ","))

event'types =
    [ "join"
    , "left"
    , "play"
    , "over"
    , "milk"
    , "talk"
    , "priv"
    ]

parseEtype = choice $ map (\n ->
    reserved n)
    event'types

parsePattern = do
    var <- parseIdentifier
    maybe <- optionMaybe (reserved ":" *> parseStringLiteral)
    case maybe of
         Just pattern -> pure $ var /\ pattern
         Nothing -> pure $ var /\ ""

parseRules = fix $ \self ->
  (parens $ fromFoldable <$>
      parsePattern `sepBy` (reserved ","))

parseTerm p = choice
    [ try $ parens p
    , try parseArray
    , try (Str <$> parseStringLiteral)
    , try (Num <$> parseNumber)
    , try (Tfv <$> parseBoolean)
    , try (Ref <$> parseVar)
    ]

bin'op'tab =
    [ [Prefix (Una <$> reserved "-")]
    , [Prefix (Una <$> reserved "!")]
    , [Infix (Bin <$> reserved "<") AssocLeft]
    , [Infix (Bin <$> reserved "<=") AssocLeft]
    , [Infix (Bin <$> reserved ">") AssocLeft]
    , [Infix (Bin <$> reserved ">=") AssocLeft]
    -- consider support in operator
    , [Infix (Bin <$> reserved "in") AssocLeft]
    , [Infix (Bin <$> reserved "==") AssocLeft]
    , [Infix (Bin <$> reserved "!=") AssocLeft]
    , [Infix (Bin <$> reserved "===") AssocLeft]
    , [Infix (Bin <$> reserved "!==") AssocLeft]
    , [Infix (Bin <$> reserved "/") AssocLeft]
    , [Infix (Bin <$> reserved "*") AssocLeft]
    , [Infix (Bin <$> reserved "-") AssocLeft]
    , [Infix (Bin <$> reserved "+") AssocLeft]
    , [Infix (Bin <$> reserved "&&") AssocRight]
    , [Infix (Bin <$> reserved "||") AssocRight]
    ]

-- try to prevent consume input
parseExpr = fix $ \self -> do
    buildExprParser bin'op'tab (parseTerm self)

expr'action = ["title", "descr", "print" ,"order"]

parseBinding =
  (parseVar >>= \var ->
      string "=" *>
      parseExpr >>= \expr ->
      pure $ Renew var expr)

parseArgument = fix $ \self ->
  try (parens $ fromFoldable <$>
      parseExpr `sepBy` (string ","))
  <|> (flip (:) []) <$> parseExpr

parseAction = fix $ \self ->
  (choice $
    map (\n ->
        Invok n <$> (reserved n *> parseArgument))
        expr'action)
  <|> Delay <$> (reserved "delay" *> parseExpr)
  <|> (do
      _ <- reserved "event"
      name <- parseEtype
      rules <- parseRules
      action <- parseAction
      pure $ Event name rules action)
  <|> try (braces $ Group <$> L.many self)
  <|> (reserved "going" *> (Going <$> parseIdentifier))
  <|> try parseBinding
  -- Timer
  -- while
  -- visit
  -- match

parseState = do
  name <- reserved "state" *>
          parseIdentifier
  action <- parseAction
  pure $ BotState name action

parseScript = let
    parseScript' = do
        actions <- (many $ parseAction)
        states <- (many $ parseState)
        case actions, states of
             [], [] -> fail "exhausted"
             as, bs -> pure $ as /\ bs in
    do xs <- many parseScript'
       pure let a /\ s = unzip xs in
           BotScript (L.fromFoldable $ concat a) (concat s)

parse p str = runParser str $ p

parseShow p ctx = case parse p ctx of
  Right actual ->
      log (show actual)
  Left err -> log ("error: " <> show err)
