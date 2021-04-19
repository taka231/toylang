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
import           Debug.Trace                    (trace)
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
defaultOP = OP $ M.fromList [
                             (7, M.fromList [("*", InfixL (ExprMul <$ symbol "*")), ("/", InfixL (ExprDiv <$ symbol "/"))])
                            ,(6, M.fromList [("+", InfixL (ExprAdd <$ symbol "+")), ("-", InfixL (ExprSub <$ symbol "-"))])
                            ,(5, M.empty)
                            ,(4, M.fromList [("==", InfixN (ExprEQ <$ symbol "=="))
                                            ,(">=", InfixN (ExprEQGT <$ symbol ">="))
                                            ,("<=", InfixN (ExprEQLT <$ symbol "<="))
                                            ,(">", InfixN (ExprGT <$ symbol ">"))
                                            ,("<", InfixN (ExprLT <$ symbol "<"))])
                            ,(3, M.empty)
                            ,(2, M.empty)
                            ,(9, M.empty)
                            ,(8, M.empty)
                            ,(1, M.empty)]

opdictToList :: OPDict -> [[Operator Parser Expr]]
opdictToList (OP opdict) = map (reverse . M.elems) (reverse (M.elems opdict))

opCall :: String -> Expr -> Expr -> Expr
opCall op expr1 expr2 = FunCall op [expr1, expr2]

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

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

infixDef :: String -> Parser ()
infixDef funName = do
  inf <- InfixL <$ symbol "infixL" <|> InfixR <$ symbol "infixR" <|> InfixN <$ symbol "infixN"
  pri <- num
  let funName' = if isOperator funName then "__OP__" ++ funName else funName
      opName = if isOperator funName then funName else '`' : funName ++ "`"
  OP opdict <- lift get
  lift $ put $ OP (M.update (Just . M.insert opName (inf (opCall funName' <$ symbol opName))) pri opdict)
  return ()
  --  inf' (try $ opCall funName' <$ symbol opName))

funInfo :: Parser Statement
funInfo = do
  funName <- parens operator <|> scIdentifier
  try $ annotation [infixDef funName]
  return Info

annotation :: [Parser ()] -> Parser ()
annotation parsers = do
  symbol "@"
  parens $ foldr (<|>) (pure ())  parsers

funDef :: Parser Statement
funDef = lexeme $ do
    funName <- (do
      op <- parens operator
      return $ "__OP__" ++ op
      ) <|> scIdentifier
    args <- some scIdentifier
    symbol "="
    FunDef funName args <$> expr

funCall :: Parser Expr
funCall = lexeme $ do
    name <- scIdentifier
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

parseWithState :: ParsecT e s (StateT OPDict Identity) a-> s -> Either (ParseErrorBundle s e) a
parseWithState parser str = evalState (runParserT parser "<stdin>" str) defaultOP

parseStatement :: String -> [Statement]
parseStatement str = case  parseWithState (sc *> statements) str of
  Right ast   -> ast
  Left bundle -> error $ errorBundlePretty bundle
