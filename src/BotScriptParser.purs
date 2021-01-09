module BotScriptParser where

import BotScript
import Data.Array hiding (null)
import Data.Foldable hiding (null)
import Data.Function
import Data.Functor
import Data.Identity
import Data.Int
import Data.Maybe
import Data.String hiding (null)
import Data.Tuple.Nested
import Prelude
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String (null)

import Control.Lazy
import Control.Applicative
import Control.Alt (alt, (<|>))
import Control.Monad.State (gets, modify_)

import CustomToken (LanguageDef, TokenParser, GenLanguageDef(..), unGenLanguageDef, makeTokenParser, alphaNum, letter, emptyDef)
import Data.Array as A
import Data.Either (Either(..))
import Data.List (List)
import Data.List as L
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (name)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Expr as P
import Text.Parsing.Parser.String (anyChar, char, eof, satisfy, oneOf)
import Undefined (undefined)
-- import Text.Parsing.Parser.Token (LanguageDef, TokenParser, GenLanguageDef(..), unGenLanguageDef, makeTokenParser, alphaNum, letter)
-- import Text.Parsing.Parser.Language (emptyDef)

customStyle :: LanguageDef
customStyle = LanguageDef (unGenLanguageDef emptyDef)
                { commentStart    = "/*"
                , commentEnd      = "*/"
                , commentLine     = "//"
                , nestedComments  = true
                , identStart      = letter <|> oneOf ['_', '$']
                , identLetter     = alphaNum <|> oneOf ['_', '$']
                , reservedNames   =
                    ["state", "true", "false", "event"
                    , "later", "going", "if", "else"
                    , "for", "while", "visit", "timer"
                    , "in", "of"
                    -- , "title", "descr" , "print" ,"order"
                    ]
                , reservedOpNames = [":", ",", "new"]
                , caseSensitive   = false
                }

tokParser = makeTokenParser customStyle
whiteSpace = tokParser.whiteSpace
angles p = tokParser.angles p
parens p = tokParser.parens p
braces p = tokParser.braces p
brackets p = tokParser.brackets p
parseIdentifier = tokParser.identifier
parseStringLiteral = tokParser.stringLiteral
parseKeyLiteral = tokParser.keyLiteral
symbol xs = tokParser.symbol xs
reserved xs = tokParser.reserved xs
reservedOp xs = tokParser.reservedOp xs
reservedOp' xs = tokParser.reservedOp xs *> pure xs

expect p = do
  consume
  val <- p
  pure val

parseBoolean =
    (reserved "true" *> pure true)
    <|> (reserved "false" *> pure false)

parseNumber =
    (try tokParser.float)
    <|> (toNumber <$> tokParser.integer)

parseArray exprP = fix $ \self ->
  (brackets $ fromFoldable >>> Arr <$>
      exprP `sepEndBy` (reservedOp ","))

event'types =
    [ "me"
    , "music"
    , "leave"
    , "join"
    , "new-host"
    , "msg"
    , "dm"
    , "dmto"
    , "submit"
    , "newtab"
    , "exittab"
    , "exitalarm"
    , "logout"
    , "musicbeg"
    , "musicend"
    , "timer"
    , "clock"
    , "kick"
    , "ban"
    , "report_and_ban_user"
    , "unban"
    , "roll"
    , "room-profile"
    , "new-description"
    , "timeout"
    ]

parseEtype = (choice $ map (\n ->
    symbol n)
    event'types) <?> show event'types

parseEtypes = (do
  (brackets $ fromFoldable <$>
      parseEtype `sepEndBy` (reservedOp ",")))
  <|> (do
      et <- parseEtype
      pure $ [et])

parsePmatch = do
    var <- try parseIdentifier
    maybe <- optionMaybe (reservedOp ":" *> parseExpr)
    case maybe of
         Just pattern -> do pure $ (true /\ var /\ pattern)
         Nothing -> pure $ (false /\ var /\ (Trm $ toTerm "String" ""))

-- () => body
parseArgs = fix $ \self -> do
  try $ reservedOp "("
  reservedOp ")"
  consume
  reservedOp "=>"
  pure $ []

-- a => body
-- a : "" => body
parseArgs'' = fix $ \self -> do
  (b /\ v /\ p) <- lookAhead parsePmatch
  if b then do
    _ <- parsePmatch
    reservedOp "=>"
    pure $ [(v /\ p)]
  else
    try (do
     _ <- parsePmatch
     reservedOp "=>"
     pure $ [(v /\ p)])

-- (a) => body
-- (a : "") => body
-- (a, b) => body
parseArgs' = fix $ \self -> do
  args <- lookAhead (parens $ fromFoldable <$>
    parsePmatch `sepEndBy` (reservedOp ","))
  case args of
    [(false /\ v /\ p)] -> try (do
      _ <- (parens $ fromFoldable <$>
        parsePmatch `sepEndBy` (reservedOp ","))
      reservedOp "=>"
      pure $ [(v /\ p)]
    )
    _ -> (do
      _ <- (parens $ fromFoldable <$>
        parsePmatch `sepEndBy` (reservedOp ","))
      consume
      reservedOp "=>"
      pure $ map (\(b /\ v /\ p) -> (v /\ p)) args
    )

parseAbs exprP = do
    args <- (parseArgs
      <|> parseArgs'
      <|> parseArgs'')
    body <- expect $ exprP <?> "Lambda Body Expr"
    pure $ Abs args body

mustLval lval =
    case lval of
        Sub _ _ -> pure lval
        Var _ -> pure lval
        Dot _ _ -> pure lval
        _ -> fail ("Expected left value, Get " <> show lval)

parseBinding exprP expr = do
    op <- reservedOp' "="
        <|> reservedOp' "+="
        <|> reservedOp' "-="
        <|> reservedOp' "*="
        <|> reservedOp' "/="
        <|> reservedOp' "%="
    lval <- mustLval expr
    rval <- exprP <?> "Binding Expression"
    P.optional (reservedOp ";")
    case op of
         "+=" -> pure $ Renew lval (Bin "+" lval expr)
         "-=" -> pure $ Renew lval (Bin "-" lval expr)
         "*=" -> pure $ Renew lval (Bin "*" lval expr)
         "/=" -> pure $ Renew lval (Bin "/" lval expr)
         "%=" -> pure $ Renew lval (Bin "%" lval expr)
         _ -> pure $ Renew lval rval

parseTerm exprP = choice
    [  parens exprP
    , (Trm <<< toTerm "String" <$> parseStringLiteral)
    , (Trm <<< toTerm "Number" <$> parseNumber)
    , (Trm <<< toTerm "Boolean" <$> parseBoolean)
    , (Var <$> parseIdentifier)
    , parseArray exprP
    ]

neg = (Una "-" <$ reservedOp "-")
not = (Una "!" <$ reservedOp "!")
new = (Una "new" <$ reservedOp "new")

inc's = (Una "_++" <$ reservedOp "++")
dec's = (Una "_--" <$ reservedOp "--")
inc'p = (Una "++_" <$ reservedOp "++")
dec'p = (Una "--_" <$ reservedOp "--")

dot = (reservedOp "." *>
       parseIdentifier >>= \attr ->
       pure $ \expr -> Dot expr attr)

sub exprP = (brackets $ exprP >>= \sub'expr ->
       pure $ \expr -> Sub expr sub'expr)

parseApp exprP = do
  args <- parens $ exprP `sepEndBy` (reservedOp ",")
  P.optional (reservedOp ";")
  pure $ \expr -> App expr (fromFoldable args)

binary name assoc =
    Infix ((Bin name) <$ reservedOp name) assoc

binary' name assoc =
    Infix ((Bin name) <$ reserved name) assoc

prefix  p = Prefix  <<< chainl1 p $ pure       (<<<)
postfix p = Postfix <<< chainl1 p $ pure (flip (<<<))

op'tab exprP =
    [ [postfix $ choice
      [parseApp exprP, dot, sub exprP, inc's, dec's]]
    , [prefix $ choice [neg, not, new, inc'p, dec'p]]
    , [binary "<" AssocLeft]
    , [binary "<=" AssocLeft]
    , [binary ">" AssocLeft]
    , [binary ">=" AssocLeft]
    , [binary' "in" AssocLeft]
    , [binary "===" AssocLeft]
    , [binary "==" AssocLeft]
    , [binary "!==" AssocLeft]
    , [binary "!=" AssocLeft]
    , [binary "/" AssocLeft]
    , [binary "%" AssocLeft]
    , [binary "*" AssocLeft]
    , [binary "-" AssocLeft]
    , [binary "+" AssocLeft]
    , [binary "&&" AssocRight]
    , [binary "||" AssocRight]
    ]

parseLval exprP = (
    buildExprParser
        [[postfix $ choice
            [dot, sub exprP]]]
        (parseTerm exprP)
)

parseSimpleExpr exprP =
  buildExprParser (op'tab exprP) (parseTerm exprP)

-- try to prevent consume input
parseExpr :: ParserT String Identity Expr
parseExpr = fix $ \self -> do
    expr <- (parseAbs self)
            <|> (do
                expr <- parseSimpleExpr self
                parseBinding self expr <|> pure expr)
            <|> parseStmtExpr self
            <|> parseObject self
            <?> "Expression"
    pure expr

-- parseKey :: ParserT String m String
parseKey = let
  folder :: Maybe Char -> L.List Char -> L.List Char
  folder Nothing chars = chars
  folder (Just c) chars = L.Cons c chars
  stringLetter = satisfy (\c -> (c /= ':') && (c /= ' ') && (c > '\x1A'))
  stringChar = (Just <$> stringLetter) in do
    maybeChars <- (L.many stringChar)
    pure $ SCU.fromCharArray $ L.toUnfoldable $ foldr folder L.Nil maybeChars


parseObjKey = do
  key <- parseStringLiteral
      <|> parseKeyLiteral
  -- key <- parseKey
  reservedOp ":"
  pure key

parseObject :: ParserT String Identity Expr -> ParserT String Identity Expr
parseObject exprP = let
    parseItem = do
       key <- try parseObjKey
       value <- expect $ exprP <?> "Object Value Expr"
       pure $ key /\ value
    parseItems = do
       items <- parseItem `sepEndBy` (reservedOp ",")
       pure $ fromFoldable items in do
          pairs <- braces parseItems
          pure $ Obj pairs

parseIfels exprP = do
  reserved "if"
  prd <- expect $ exprP <?> "If Pred"
  expect $ reserved "then"
  thn <- expect $ exprP <?> "Then Expr"
  maybe <- optionMaybe (reserved "else")
  case maybe of
     Just _ -> (do
       els <- expect $ exprP <?> "Else Expr"
       pure $ Ifels prd thn els)
     Nothing -> pure $ Ifels prd thn (Group L.Nil)

parseTimer exprP = do
  reserved "timer"
  prd <- expect $ exprP <?> "Timer Period"
  act <- expect $ exprP <?> "Timer Expr"
  pure $ Timer prd act

parseWhile exprP = do
  reserved "while"
  prd <- expect $ exprP <?> "While Pred"
  act <- expect $ exprP <?> "While Expr"
  pure $ While prd act

parseLater exprP = do
  reserved "later"
  time <- expect $ exprP <?> "Later Period"
  expr <- expect $ exprP <?> "Later Expr"
  pure $ Later time expr

parseEvent exprP = do
  reserved "event"
  name <- expect $ parseEtypes
  expr <- expect $ exprP <?> "Event Expr"
  pure $ Event name expr

parseFLoop exprP = let
  desc = do
     init <- expect $ exprP <?> "For Init Expr"
     cond <- expect $ exprP <?> "For Cond Expr"
     step <- expect $ exprP <?> "For Step Expr"
     pure $ init /\ cond /\ step in do
        reserved "for"
        init /\ cond /\ step <- (parens desc <|> desc)
        body <- expect $ exprP <?> "For Body Expr"
        pure $ Group (L.fromFoldable
         [init, While cond
         (Group (L.fromFoldable [body, step]))])

parseForIn exprP = let
  desc = do
     var <- parseLval exprP <?> "For Var"
     var' <- mustLval var
     reserved "in"
     iter <- expect $ exprP <?> "For Iter"
     pure $ var' /\ iter in do
        reserved "for"
        var /\ iter <- (parens desc <|> desc)
        body <- expect $ exprP <?> "For Body Expr"
        pure $ Group (L.fromFoldable
         [ (Renew (Var "@iter")
            (App (Dot
                (App (Dot
                     (Var "Object") "keys") [iter]
                ) "values") [])
            )
         , (Renew (Var "@it")
            (App (Dot (Var "@iter") "next") []))
         , While (Una "!" (Dot (Var "@it") "done"))
         (Group (L.fromFoldable
            [ (Renew var (Dot (Var "@it") "value"))
            , body
            , (Renew (Var "@it")
                (App (Dot (Var "@iter") "next") []))
            ]))
         ])

parseForOf exprP = let
  desc = do
     var <- parseLval exprP <?> "For Var"
     var' <- mustLval var
     reserved "of"
     iter <- expect $ exprP <?> "For Iter"
     pure $ var' /\ iter in do
        reserved "for"
        var /\ iter <- (parens desc <|> desc)
        body <- expect $ exprP <?> "For Body Expr"
        pure $ Group (L.fromFoldable
         [ (Renew (Var "@iter")
            (App (Dot
                (App (Dot
                    (Var "Object") "values") [iter])
                "values") []))
         , (Renew (Var "@it")
            (App (Dot (Var "@iter") "next") []))
         , While (Una "!" (Dot (Var "@it") "done"))
         (Group (L.fromFoldable
            [ (Renew var (Dot (Var "@it") "value"))
            , body
            , (Renew (Var "@it")
                (App (Dot (Var "@iter") "next") []))
            ]))
         ])

parseStmt exprP
  = parseIfels exprP
  <|> parseTimer exprP
  <|> parseWhile exprP
  <|> parseLater exprP
  <|> parseEvent exprP
  <|> parseForIn exprP
  <|> parseForOf exprP
  <|> parseFLoop exprP
  <|> (reserved "going" *> (Going <$> expect parseIdentifier))
  <|> (reserved "visit" *> (Visit <$> parseIdentifier))
  <|> (do
      exprs <- braces $ L.many exprP
      case exprs of
           L.Nil -> pure $ Obj []
           _ -> pure $ Group exprs)

parseStmtExpr :: ParserT String Identity Expr -> ParserT String Identity Expr
parseStmtExpr exprP = (do
  stmtExpr <- parseStmt exprP
  P.optional (reservedOp ";")
  pure stmtExpr)
  <|> ((Group L.Nil) <$ reservedOp ";")
  <?> "Statement Expression"

parseState = do
  reserved "state"
  name <- (parseIdentifier <?> "State Name")
  expr <- parseExpr <?> "State Expr"
  pure $ BotState name expr

testParseStates = do
    lookAhead (reserved "state")
    (some parseState >>= \states -> pure (true /\ states))
    <|> pure (false /\ [])

testParseExprs =
    (some parseExpr >>= \exprs ->
        pure (true /\ exprs))
    <|> pure (false /\ [])

parseScript' = do
        (sr /\ states) <- testParseStates
        (ar /\ exprs) <- testParseExprs
        if sr || ar then
            pure $ states /\ exprs
        else fail "Expected State or Expression"

anyToken = do
  chars <- many (satisfy \c -> (c /= ' '))
  pure $ SCU.fromCharArray chars

parseScript = do
    whiteSpace
    xs <- many parseScript'
    (do
      unexp <- anyToken
      fail $ ("Unexpected token: " <> unexp)) <|> eof
    pure let s /\ a = unzip xs in
        BotScript (L.fromFoldable $ concat a) (concat s)

parse p str = runParser str $ p

parseShow p ctx = case parse p ctx of
  Right actual ->
      log (show actual)
  Left err -> log ("error: " <> show err)
