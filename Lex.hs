module Lex( lexer, whiteSpace ) where

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
double	  = P.float lexer
stringLit = P.stringLiteral lexer
parens    = P.parens lexer
braces    = P.braces lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
comma     = P.comma lexer

data Expr = 
    CInt Int
  | CDouble Double
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

getType = do
		reserved "int" 
		return Int 
	<|> do
		reserved "double"
		return Double
	<|> do
		reserved "string"
		return String

getParams = do
		t <- getType
		i <- identifier
		comma
		p <- getParams
		return $ (Param t i):p
	<|> do
		t <- getType
		i <- identifier
		return $ [(Param t i)]
	<|> do
		return []

getFuncArgs = do		
		e <- expr
		comma
		a <- getFuncArgs
		return $ (Arg e):a
	<|> do
		e <- expr
		return $ [(Arg e)]
	<|> do
		return []
		 		

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
    f <- double
    return $ CDouble f
  <|> do
    s <- stringLit
    return $ CString s
  <|> do
    v <- identifier
    return $ Var v
  <|> parens expr
  <?> "term"

-- 

data Cmd =
	  Empty
	| IfStmt Expr Cmd Cmd
	| WhileStmt Expr Cmd
	| Func Type String [Param] Cmd	-- definicia funkcie
	| FuncDecl Type String [Param]
	| FuncCall String [Arg]				-- volanie funkcie
	| AssignStmt String Expr					-- priradenie premennej
	| VarDefStmt Type String					-- deklaracia premennej v bloku
	| ReturnStmt Expr
	| Print Expr 
	| Scan String
	| Seq [Cmd]

data Type = 
	  String
	| Int
	| Double

data Param = Param Type String

data Arg = Arg Expr

command = 
	do
		semi
		return Empty
	<|>	do
		t <- getType 
		i <- identifier
		semi
		return $ VarDefStmt t i
	<|> do
		t <- getType
		i <- identifier
		params <- parens $ getParams 
		semi
		return $ FuncDecl t i params	
	<|> do
		t <- getType
		i <- identifier
		params <- parens $ getParams 
		c <- command
		return $ Func t i params c
	<|> do
		i <- identifier
		a <- getFuncArgs
		semi
		return $ FuncCall i a
	<|> do
		reserved "return"
		e <- expr
		return $ ReturnStmt e		
	<|> do
    	reserved "print"
    	e <- expr
    	semi
    	return $ Print e
  	<|> do
    	reserved "scan"
    	i <- identifier
    	semi
    	return $ Scan i
	<|> do
		i <- identifier
		reservedOp "="
		e <- expr
   		semi
		return $ AssignStmt i e
  	<|> do
		reserved "if"
		b <- expr    	
		c1 <- command
		reserved "else"
		c2 <- command
		return $ IfStmt b c1 c2
  	<|> do
		reserved "while"
		b <- expr    
		c <- command
		return $ WhileStmt b c
  	<|> do
		seq <- braces $ many command
		return $ Seq seq
  	<?> "command"






--end demo.hs
