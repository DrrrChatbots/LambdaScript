module BotScriptParser where

import BotScript
import Control.Lazy
import Data.Array
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Identity
import Data.Int
import Data.Maybe
import Data.String
import Data.Tuple.Nested
import Prelude
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr

import Control.Alt ((<|>))
import Data.Array as A
import Data.Either (Either(..))
import Data.List (List)
import Data.List as L
import Data.String.CodeUnits (fromCharArray, singleton)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (name)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Expr as P
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (anyChar, char, eof, satisfy, string)
import Text.Parsing.Parser.String (char, oneOf)
import Text.Parsing.Parser.Token (LanguageDef, TokenParser, GenLanguageDef(..), unGenLanguageDef, makeTokenParser, alphaNum, letter)
import Text.Parsing.Parser.Token (makeTokenParser)
import Undefined (undefined)

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
angles p = whiteSpace *> tokParser.angles p
parens p = whiteSpace *> tokParser.parens p
braces p = whiteSpace *> tokParser.braces p
brackets p = whiteSpace *> tokParser.brackets p
parseIdentifier = whiteSpace *> tokParser.identifier
parseStringLiteral = whiteSpace *> tokParser.stringLiteral
symbol xs = whiteSpace *> tokParser.symbol xs
reserved xs = whiteSpace *> tokParser.reserved xs
reservedOp xs = whiteSpace *> tokParser.reservedOp xs
reservedOp' xs = whiteSpace *> tokParser.reservedOp xs *> pure xs

parseBoolean = whiteSpace *>
    (reserved "true" *> pure true)
    <|> (reserved "false" *> pure false)

parseNumber = whiteSpace *>
    (try tokParser.float)
    <|> (toNumber <$> tokParser.integer)

parseArray exprP = fix $ \self ->
  (brackets $ fromFoldable >>> Arr <$>
      exprP `sepEndBy` (string ","))

-- event'types =
--     [ "join"
--     , "left"
--     , "play"
--     , "over"
--     , "milk"
--     , "talk"
--     , "priv"
--     ]

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
      parseEtype `sepEndBy` (string ",")))
  <|> (do
      et <- parseEtype
      pure $ [et])

parsePmatch = do
    var <- parseIdentifier
    maybe <- optionMaybe (reserved ":" *> parseExpr)
    case maybe of
         Just pattern -> pure $ var /\ pattern
         Nothing -> pure $ var /\ (Trm $ toTerm "String" "")

parseArguments = fix $ \self ->
  (parens $ fromFoldable <$>
      parsePmatch `sepEndBy` (reserved ","))
  <|> (flip A.(:) [] <$> parsePmatch)

parseAbsHead parseArgs = do
    args <- parseArgs
    reserved "=>"
    pure args

parseAbs exprP = do
    args <- try $ parseAbsHead parseArguments
    body <- exprP
    pure $ Abs args body

mustLval lval =
    case lval of
        Sub _ _ -> pure lval
        Var _ -> pure lval
        Dot _ _ -> pure lval
        _ -> fail "Expected left value"

parseBinding exprP expr = do
    whiteSpace
    op <- reservedOp' "="
        <|> reservedOp' "+="
        <|> reservedOp' "-="
        <|> reservedOp' "*="
        <|> reservedOp' "/="
        <|> reservedOp' "%="
    lval <- mustLval expr
    rval <- exprP <?> "Binding Expression"
    P.optional (reserved ";")
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

parseApp exprP = try do
    args <- parens $ exprP `sepEndBy` (string ",")
    maybe <- optionMaybe (reserved "=>")
    case maybe of
       Just _ -> fail "reserved for lambda"
       Nothing -> pure $ \expr -> App expr (fromFoldable args)


binary name assoc =
    Infix ((Bin name) <$ reservedOp name) assoc

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
    , [binary "in" AssocLeft]
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

parseLval exprP = whiteSpace *> (
    buildExprParser
        [[postfix $ choice
            [dot, sub exprP]]]
        (parseTerm exprP)
)

-- try to prevent consume input
parseExpr :: ParserT String Identity Expr
parseExpr = fix $ \self -> whiteSpace *> (
    let exprP = buildExprParser
                    (op'tab self)
                    (parseTerm self) in do
    (parseAbs self)
    <|> (do
        expr <- exprP
        parseBinding self expr <|> pure expr
        )
    <|> (parseObject self)
    <|> parseStmtExpr self
    <?> "Expression"
)

parseObject :: ParserT String Identity Expr -> ParserT String Identity Expr
parseObject exprP = let
    parseItem = do
       key <- parseIdentifier
       reserved ":"
       value <- exprP
       pure $ key /\ value
    parseItems = do
       items <- parseItem `sepEndBy` (string ",")
       pure $ fromFoldable items in do
          pairs <- braces $ parseItems
          pure $ Obj pairs

parseStmtExpr :: ParserT String Identity Expr -> ParserT String Identity Expr
parseStmtExpr exprP = (do
  stmtExpr <- (
      (braces $ Group <$> L.many exprP)
      <|> (do
          reserved "later"
          time <- exprP
          expr <- exprP
          pure $ Later time expr)
      <|> (do
          reserved "event"
          name <- parseEtypes
          expr <- exprP
          pure $ Event name expr)
      <|> (do
          reserved "if"
          prd <- exprP <?> "If Pred"
          thn <- exprP <?> "Then Expr"
          maybe <- optionMaybe (reserved "else")
          case maybe of
             Just _ -> (do
               els <- exprP <?> "Else Expr"
               pure $ Ifels prd thn els)
             Nothing -> pure $ Ifels prd thn (Group L.Nil))
      <|> try (let -- for loop
          desc = do
             init <- exprP <?> "For Init Expr"
             cond <- exprP <?> "For Cond Expr"
             P.optional (reserved ";")
             step <- exprP <?> "For Step Expr"
             pure $ init /\ cond /\ step in do
                reserved "for"
                init /\ cond /\ step <- (parens desc <|> desc)
                body <- exprP <?> "For Body Expr"
                pure $ Group (L.fromFoldable
                 [init, While cond
                 (Group (L.fromFoldable [body, step]))])
            )
      <|> (let -- for iter in
          desc = do
             whiteSpace
             var <- parseLval exprP <?> "For Var"
             var' <- mustLval var
             reserved "in"
             iter <- exprP <?> "For Iter"
             pure $ var' /\ iter in do
                reserved "for"
                var /\ iter <- (parens desc <|> desc)
                body <- exprP <?> "For Body Expr"
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
            )
      <|> (let -- for of
          desc = do
             whiteSpace
             var <- parseLval exprP <?> "For Var"
             var' <- mustLval var
             reserved "of"
             iter <- exprP <?> "For Iter"
             pure $ var' /\ iter in do
                reserved "for"
                var /\ iter <- (parens desc <|> desc)
                body <- exprP <?> "For Body Expr"
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
            )
      <|> (do
          reserved "while"
          prd <- exprP <?> "Pred"
          act <- exprP <?> "While Expr"
          pure $ While prd act)
      <|> (reserved "going" *> (Going <$> parseIdentifier))
      <|> (reserved "visit" *> (Visit <$> parseIdentifier))
      <|> (do
          reserved "timer"
          prd <- exprP <?> "Timer Period"
          act <- exprP <?> "Timer Expr"
          pure $ Timer prd act)
  ) -- may consume, need restore
  P.optional (reserved ";")
  pure stmtExpr)
  <|> ((Group L.Nil) <$ reserved ";")

parseState = do
  reserved "state"
  name <- (parseIdentifier <?> "State Name")
  expr <- parseExpr
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
        whiteSpace
        (sr /\ states) <- testParseStates
        whiteSpace
        (ar /\ exprs) <- testParseExprs
        if sr || ar then
            pure $ states /\ exprs
        else fail "Expected State or Expression"

parseScript = do
    whiteSpace
    xs <- many parseScript'
    eof
    pure let s /\ a = unzip xs in
        BotScript (L.fromFoldable $ concat a) (concat s)

parse p str = runParser str $ p

parseShow p ctx = case parse p ctx of
  Right actual ->
      log (show actual)
  Left err -> log ("error: " <> show err)
