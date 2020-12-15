module Main where

import Data.Array
import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.String.CodeUnits (fromCharArray, singleton)
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.String (anyChar, char, eof, satisfy, string, whiteSpace)
import Text.Parsing.Parser.Token (makeTokenParser)

tokParser = makeTokenParser javaStyle

parseIdentifier = tokParser.whiteSpace *> tokParser.identifier

parseFunction =
  string "title"
  <|> string "print"

parseArgument =
  tokParser.stringLiteral
  <|> (tokParser.decimal >>= \v -> pure (show v))
  <|> (string "\n" >>= \v -> pure "void")

parseInst = do
  func <- parseFunction
  pure {func: func}
  -- args <- many parseArgument
  -- pure {func: func, args: args}

parseState = do
  name <- string "state" *>
          parseIdentifier
  inst <- tokParser.braces $ many parseIdentifier
  pure {state: name, insts: inst}
  -- name <- many anyChar
  -- pure $ fromCharArray name

parse str = case runParser str $ many parseState of
  Right actual ->
      log (show actual)
  Left err -> log ("error: " <> show err)

main :: Effect Unit
main = parse "state hello {  title title print }"
