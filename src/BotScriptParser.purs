module BotScriptParser where

import BotScript
import Control.Lazy
import Data.Array
import Data.Identity
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
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (anyChar, char, eof, satisfy, string)
import Text.Parsing.Parser.String (char, oneOf)
import Text.Parsing.Parser.Token (LanguageDef, TokenParser, GenLanguageDef(..), unGenLanguageDef, makeTokenParser, alphaNum, letter)
import Text.Parsing.Parser.Token (makeTokenParser)

customStyle :: LanguageDef
customStyle = LanguageDef (unGenLanguageDef emptyDef)
                { commentStart    = "/*"
                , commentEnd      = "*/"
                , commentLine     = "//"
                , nestedComments  = true
                , identStart      = letter <|> oneOf ['_', '$']
                , identLetter     = alphaNum <|> oneOf ['_', '$']
                , reservedNames   = ["state", "true", "false", "event"
                                    , "delay", "going"
                                    , "title", "descr", "print" ,"order"]
                , reservedOpNames = [":", ","]
                , caseSensitive   = false
                }

tokParser = makeTokenParser customStyle
whiteSpace = tokParser.whiteSpace
parens p = whiteSpace *> tokParser.parens p
braces p = whiteSpace *> tokParser.braces p
brackets p = whiteSpace *> tokParser.brackets p
parseIdentifier = whiteSpace *> tokParser.identifier
parseStringLiteral = whiteSpace *> tokParser.stringLiteral
symbol xs = whiteSpace *> tokParser.symbol xs
reserved xs = whiteSpace *> tokParser.reserved xs

parseBoolean = whiteSpace *>
    (reserved "true" *> pure true)
    <|> (reserved "false" *> pure false)

parseNumber = whiteSpace *>
    (try tokParser.float)
    <|> (toNumber <$> tokParser.integer)

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
    symbol n)
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
    [ parseArray
    , (Trm <<< toTerm "String" <$> parseStringLiteral)
    , (Trm <<< toTerm "Number" <$> parseNumber)
    , (Trm <<< toTerm "Boolean" <$> parseBoolean)
    , (do
        _ <- symbol "`"
        name <- parseIdentifier `sepBy` (string ".")
        args <- parseArgument
        pure (Fun (fromFoldable name) args))
    , (Ref <$> parseVar)
    ,  parens p
    ]

bin'op'tab =
    [ [Prefix (Una <$> symbol "-")]
    , [Prefix (Una <$> symbol "!")]
    , [Infix (Bin <$> symbol "<") AssocLeft]
    , [Infix (Bin <$> symbol "<=") AssocLeft]
    , [Infix (Bin <$> symbol ">") AssocLeft]
    , [Infix (Bin <$> symbol ">=") AssocLeft]
    -- consider support in operator
    , [Infix (Bin <$> symbol "in") AssocLeft]
    , [Infix (Bin <$> symbol "==") AssocLeft]
    , [Infix (Bin <$> symbol "!=") AssocLeft]
    , [Infix (Bin <$> symbol "===") AssocLeft]
    , [Infix (Bin <$> symbol "!==") AssocLeft]
    , [Infix (Bin <$> symbol "/") AssocLeft]
    , [Infix (Bin <$> symbol "%") AssocLeft]
    , [Infix (Bin <$> symbol "*") AssocLeft]
    , [Infix (Bin <$> symbol "-") AssocLeft]
    , [Infix (Bin <$> symbol "+") AssocLeft]
    , [Infix (Bin <$> symbol "&&") AssocRight]
    , [Infix (Bin <$> symbol "||") AssocRight]
    ]

-- try to prevent consume input
parseExpr = fix $ \self ->
    whiteSpace *> do
    buildExprParser bin'op'tab (parseTerm self)
    <|> fail "Expected Expression"

expr'action = ["title", "descr", "print" ,"order"]

testBind = try (do
    var <- parseVar
    whiteSpace
    reserved "="
    pure var
)

parseBinding = do
    var <- testBind
    expr <- parseExpr <|> fail "Expected Binding Expression"
    pure $ Renew var expr

parseArgument = fix $ \self ->
  (parens $ fromFoldable <$>
      parseExpr `sepBy` (string ","))
  <|> (flip (:) []) <$> parseExpr

parseAction::ParserT String Identity Action
parseAction = fix $ \self ->
  ((choice $
    map (\n ->
        Invok [n] <$> (reserved n *> parseArgument))
        expr'action)
  <|> Delay <$> (reserved "delay" *> parseExpr)
  <|> (do
        _ <- symbol "`"
        name <- parseIdentifier `sepBy` (string ".")
        args <- parseArgument
        pure (Invok (fromFoldable name) args))
  <|> (do
      reserved "event"
      name <- parseEtype
      rules <- parseRules
      action <- parseAction
      pure $ Event name rules action)
  <|> (braces $ Group <$> L.many self)
  <|> (reserved "going" *> (Going <$> parseIdentifier))
  <|> parseBinding) -- may consume, need restore
  -- Timer
  -- while
  -- visit
  -- match

parseState = do
  reserved "state"
  name <- (parseIdentifier <|> fail "Expected State Name")
  action <- parseAction
  pure $ BotState name action

testParseStates =
    (some parseState >>= \states -> pure (true /\ states))
    <|> pure (false /\ [])

testParseActions =
    (some parseAction >>= \actions ->
        pure (true /\ actions))
    <|> pure (false /\ [])

parseScript' = do
        whiteSpace
        (sr /\ states) <- testParseStates
        whiteSpace
        (ar /\ actions) <- testParseActions
        if sr || ar then
            pure $ states /\ actions
        else fail "Expected State or Action"

parseScript = do
    xs <- some parseScript'
    eof
    pure let s /\ a = unzip xs in
        BotScript (L.fromFoldable $ concat a) (concat s)

parse p str = runParser str $ p

parseShow p ctx = case parse p ctx of
  Right actual ->
      log (show actual)
  Left err -> log ("error: " <> show err)
