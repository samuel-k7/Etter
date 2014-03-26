module Lex( lexer ) where

import System.Environment( getArgs )
import System.IO

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

aelDef = emptyDef
	{ commentStart   = "/*"
	, commentEnd     = "*/"
	, commentLine    = "//"
	, nestedComments = False
	, identStart     = letter <|> char '_'
	, identLetter    = alphaNum <|> char '_'
	, opStart        = oneOf "=+*-!><"
	, opLetter       = opStart aelDef
	, reservedOpNames= [ "=", "+", "*", "-", "/", "==", "!=", "<", "<=", ">=", ">" ]
	, reservedNames  = [ "double", "else", "if", "int", "print", "scan", "string", "while" ]
	, caseSensitive  = True
	}

lexer = P.makeTokenParser aelDef

whiteSpace= P.whiteSpace lexer
integer   = P.integer lexer
float	  = P.float lexer
stringLit = P.stringLiteral lexer
parens    = P.parens lexer
braces    = P.braces lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer

data Expr = 
    CInt Int
  | CFloat Double
  | CString String
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Gt Expr Expr
  | GtEq Expr Expr
  | Lt Expr Expr
  | LtEq Expr Expr
  | Eq Expr Expr
  | Neq Expr Expr
  deriving Show

expr = buildExpressionParser operators term where
  operators = [
      [ op "*" Mult, op "/" Div ],
      [ op "+" Add, op "-" Sub ],
	  [ op "<" Lt, op "<=" LtEq, op ">" Gt, op ">=" GtEq, op "==" Eq, op "!=" Neq]
    ]
  op name fun =
    Infix ( do { reservedOp name; return fun } ) AssocLeft

term = do
    i <- integer
    return $ CInt $ fromInteger i
  <|> do
    f <- float
    return $ CFloat f
  <|> do
    s <- stringLit
    return $ CString s
  <|> do
    v <- identifier
    return $ Var v
  <|> parens expr
  <?> "term"

--end demo.hs
