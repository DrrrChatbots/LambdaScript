module Test.Main where

import Prelude (class Eq, class Show, Unit, discard, pure, show, ($), (*>), (<>), (==), (>>=), (>>>))

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (logShow)
import Test.Assert (assert')
import Text.Parsing.Parser (Parser, runParser, parseErrorPosition)
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.Token (makeTokenParser)

import BotScriptParser

mkPos :: Int -> Position
mkPos n = mkPos' n 1

mkPos' :: Int -> Int -> Position
mkPos' column line = Position { column: column, line: line }


type TestM = Effect Unit

parseErrorTestPosition :: forall s a. Show a => Parser s a -> s -> Position -> Effect Unit
parseErrorTestPosition p input expected = case runParser input p of
  Right _ -> assert' "error: ParseError expected!" false
  Left err -> do
    let pos = parseErrorPosition err
    assert' ("expected: " <> show expected <> ", pos: " <> show pos) (expected == pos)
    logShow expected



parseTest :: forall s a. Show a => Eq a => s -> a -> Parser s a -> Effect Unit
parseTest input expected p = case runParser input p of
  Right actual -> do
    assert' ("expected: " <> show expected <> ", actual: " <> show actual) (expected == actual)
    logShow actual
  Left err -> assert' ("error: " <> show err) false

javaStyleTest :: TestM
javaStyleTest = do
    let javaTokParser = makeTokenParser javaStyle
    -- make sure java-style comments work
    parseTest "hello /* comment\n */ fo_" "fo_" $ javaTokParser.identifier *> javaTokParser.identifier

    -- make sure java-style identifier work
    parseTest "$hello /* comment\n */ _f$o_" "_f$o_" $ javaTokParser.identifier *> javaTokParser.identifier

    -- make sure haskell-style comments do not work
    parseErrorTestPosition
        (javaTokParser.identifier *> javaTokParser.identifier)
        "hello {- comment\n -} foo"
        (mkPos 7)

botScriptTest :: TestM
botScriptTest = do
    parseTest "title \"hello\"" "(Title \"hello\")" $
        parseExpr >>= show >>> pure

main :: Effect Unit
main = do
  javaStyleTest
  botScriptTest

