module Parse where

import           AST
import           Control.Monad.Combinators.Expr
import           Control.Monad.State            (lift)
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import qualified Data.Map                       as M
import qualified Data.Text                      as DT
import           Data.Text.Internal.Lazy
import qualified Data.Text.IO                   as T
import qualified Data.Text.Lazy.IO              as LT
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Debug          (dbg)

newtype OPDict = OP (M.Map Integer (M.Map String (Operator Parser Expr) ))

-- type Parser = StateT OPDict (Parsec Void String)
type Parser = ParsecT Void String (Control.Monad.Trans.State.State OPDict)

-- defaultOP :: OPDict
-- defaultOP = OP $ M.fromList [(7, M.fromList [("*", InfixL (opCall "__OP__*" <$ symbol "*")), ("/", InfixL (opCall "__OP__/" <$ symbol "/"))])
--                             ,(6, M.fromList [("+", InfixL (opCall "__OP__+" <$ symbol "+")), ("-", InfixL (opCall "__OP__-" <$ symbol "-"))])
--                             ,(4, M.fromList [("==", InfixN (opCall "__OP__==" <$ symbol "=="))
--                                             ,(">=", InfixN (opCall "__OP__>=" <$ symbol ">="))
--                                             ,("<=", InfixN (opCall "__OP__<=" <$ symbol "<="))
--                                             ,(">", InfixN (opCall "__OP__>" <$ symbol ">"))
--                                             ,("<", InfixN (opCall "__OP__<" <$ symbol "<"))])]

defaultOP :: OPDict
defaultOP = OP $ M.fromList [(7, M.fromList [("*", InfixL (ExprMul <$ symbol "*")), ("/", InfixL (ExprDiv <$ symbol "/"))])
                            ,(6, M.fromList [("+", InfixL (ExprAdd <$ symbol "+")), ("-", InfixL (ExprSub <$ symbol "-"))])
                            ,(4, M.fromList [("==", InfixN (ExprEQ <$ symbol "=="))
                                            ,(">=", InfixN (ExprEQGT <$ symbol ">="))
                                            ,("<=", InfixN (ExprEQLT <$ symbol "<="))
                                            ,(">", InfixN (ExprGT <$ symbol ">"))
                                            ,("<", InfixN (ExprLT <$ symbol "<"))])]

opdictToList :: OPDict -> [[Operator Parser Expr]]
opdictToList (OP opdict) = map (reverse . M.elems) (reverse (M.elems opdict))

opCall :: String -> Expr -> Expr -> Expr
opCall op expr1 expr2 = FunCall op [expr1, expr2]

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = do
  firstLetter <- letterChar
  middleLetters <- many ( oneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']) )
  lastLetters <- many (oneOf "_'")
  pure $ firstLetter : (middleLetters ++ lastLetters)

operator :: Parser String
operator = some (oneOf ['!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '^', '|', '-', '~'])

isOperator :: String -> Bool
isOperator = all (\char -> char `elem` ['!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '^', '|', '-', '~'])

scIdentifier :: Parser String
scIdentifier = lexeme identifier

num :: Parser Integer
num = lexeme L.decimal

op :: Char -> String -> (String, Operator Parser Expr)
op inf funName= let
  inf' | inf == 'L' = InfixL
       | inf == 'R' = InfixR
       | otherwise = InfixN
  funName' = if isOperator funName then "__OP__" ++ funName else funName
  opName = if isOperator funName then funName else '`' : funName ++ "`"
  in
   (opName, inf' (opCall funName' <$ symbol opName))

ops :: [[Operator Parser Expr]]
ops =
  [
    [ InfixL (ExprMul <$ symbol "*")   -- 掛け算は左結合で、文字は *
    , InfixL (ExprDiv <$ symbol "/") ],-- 割り算も同様
    [ InfixL (ExprAdd <$ symbol "+")
    , InfixL (ExprSub <$ symbol "-") ],
    [ InfixL (ExprEQ <$ symbol "==")
    , InfixL (ExprEQGT <$ symbol ">=")
    , InfixL (ExprEQLT <$ symbol "<=")
    , InfixL (ExprGT <$ symbol ">")
    , InfixL (ExprLT <$ symbol "<") ]
  ]

typeSig :: Parser Type
typeSig =
  undefined

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

infixDef :: String -> Parser (Integer, (String, Operator Parser Expr))
infixDef funName = do
  symbol "infix"
  inf <- char 'L' <|> char 'R' <|> char 'N'
  some $ char ' '
  pri <- num
  return (pri, op inf funName)

funInfo :: Parser Statement
funInfo = do
  funName <- (do
    symbol "("
    op <- operator
    symbol ")"
    return op) <|> scIdentifier
  try $ do
    char '@'
    char '('
    (pri, (opname, opInfix)) <- infixDef funName
    char ')'
    OP opdict <- lift get
    lift $ put $ OP (M.update (Just . M.insert opname opInfix) pri opdict)
  return Info

funDef :: Parser Statement
funDef = lexeme $ do
    funName <- (do
      symbol "("
      op <- operator
      symbol ")"
      return $ "__OP__" ++ op
      ) <|> scIdentifier
    args <- some scIdentifier
    symbol "="
    FunDef funName args <$> expr

funCall :: Parser Expr
funCall = lexeme $ do
    name <- identifier
    some $ char ' '
    args <- some $ Var <$> scIdentifier <|> term
    pure $ FunCall name args

statement :: Parser Statement
statement = try funInfo <|> try funDef <|> try assign <|> StateExpr <$> expr

expr :: Parser Expr
expr = do
  opdict <- lift get
  makeExprParser term (opdictToList opdict)

term :: Parser Expr
term = ExprInt <$> num <|> parens expr <|> exprIf <|> try funCall <|> Var <$> scIdentifier

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

statements :: Parser [Statement]       -- 行の区切りは ';'
statements = statement `sepEndBy` symbol ";"

parseStatement :: String -> [Statement]
parseStatement str = case evalState (runParserT (sc *> statements) "<stdin>" str) defaultOP of
  Right ast   -> ast
  Left bundle -> error $ errorBundlePretty bundle
