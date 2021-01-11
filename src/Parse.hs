module Parse where

import AST
import Control.Monad.Combinators.Expr
import Data.Text.Internal.Lazy
import Data.Functor.Identity
import Data.Void
import qualified Data.Text as DT
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = do
  firstLetter <- letterChar
  middleLetters <- many ( oneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']) )
  lastLetters <- many (oneOf "!?_'")
  pure $ firstLetter : (middleLetters ++ lastLetters)

scIdentifier :: Parser String
scIdentifier = lexeme identifier

num :: Parser Integer
num = lexeme L.decimal

ops :: [[Operator Parser Expr]]
ops =
  [
    [ InfixL (ExprMul <$ symbol "*")   -- 掛け算は左結合で、文字は *
    , InfixL (ExprDiv <$ symbol "/") ],-- 割り算も同様
    [ InfixL (ExprAdd <$ symbol "+")
    , InfixL (ExprSub <$ symbol "-") ],
    [ InfixL (ExprEQGT <$ symbol ">=")
    , InfixL (ExprEQLT <$ symbol "<=")
    , InfixL (ExprGT <$ symbol ">")
    , InfixL (ExprLT <$ symbol "<") ]
  ]

exprIf :: Parser Expr
exprIf = do
    symbol "if"        -- まず "if" を読み
    condExpr <- expr   -- 次に現れる式を条件式とする
    symbol "then"      -- "then" を読み
    thenExpr <- expr   -- 次の式を then式 とする
    symbol "else"      -- "else" も同様
    ExprIf condExpr thenExpr <$> expr -- 全体を ExprIf でくるむ

assign :: Parser Statement
assign = do
  variname <- Var <$> scIdentifier -- 変数名を見つけたら Var にくるむ
  symbol "="
  Assign variname <$> expr

funDef :: Parser Statement
funDef = dbg "funDef" $ lexeme $ do
    funName <- identifier
    args <- some $ try $ do
      some $ char ' '
      identifier
    many $ char ' '
    symbol "="
    FunDef funName args <$> expr

funCall :: Parser Expr
funCall = dbg "funCall" $ lexeme $ do
    name <- identifier
    some $ char ' '
    args <- some $ Var <$> scIdentifier <|> expr
    pure $ FunCall name args

statement :: Parser Statement
statement = try funDef <|> try assign <|> StateExpr <$> expr

expr :: Parser Expr
expr = makeExprParser term ops

term :: Parser Expr
term = ExprInt <$> num <|> parens expr <|> exprIf <|> try funCall <|> Var <$> scIdentifier

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

statements :: Parser [Statement]       -- 行の区切りは ';'
statements = statement `sepEndBy` symbol ";"

parseStatement :: String -> [Statement]
parseStatement str = case parse (sc *> statements) "<stdin>" str of
  Right ast -> ast
  Left bundle -> error $ errorBundlePretty bundle