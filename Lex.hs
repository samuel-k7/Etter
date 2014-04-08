module Lex( lexer, whiteSpace ) where

import System.Environment( getArgs )
import System.IO

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

-- Lexikalna analyza
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

-- Lexikalny analyzator
lexer = P.makeTokenParser aelDef

-- Pomocne funkcie lexikalnej analyzy
whiteSpace= P.whiteSpace lexer
integer   = P.integer lexer
double    = P.float lexer
stringLit = P.stringLiteral lexer
parens    = P.parens lexer
braces    = P.braces lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
comma     = P.comma lexer

-- Definicia hodnot
data Value = 
      ValInt Int
    | ValDouble Double
    | ValString String
    deriving (Show, Eq, Ord)

-- Definicia vyrazov
data Expr =
    Const Value
  | Var String
  | Fun String [Arg]
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

-- Ziskanie typu premennej, funkcie
getType = do
        reserved "int"
        return Int
    <|> do
        reserved "double"
        return Double
    <|> do
        reserved "string"
        return String

-- Ziskanie paramatrov funkcie pri jej deklaracii, definicii
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

-- Ziskanie argumentov funkcie pri jej volani
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

-- Precedencna analyza vyrazov
expr = buildExpressionParser operators term where
  operators = [
      [ op "*" Mult, op "/" Div ],
      [ op "+" Add, op "-" Sub ],
      [ op "<" Lt, op "<=" LtEq, op ">" Gt, op ">=" GtEq, op "==" Eq, op "!=" Neq]
    ]
  op name fun =
    Infix ( do { reservedOp name; return fun } ) AssocLeft

-- Spracovanie vyrazu pomocou definovanych datovych typov
term = do
    i <- integer
    return $ Const $ ValInt $ fromInteger i
  <|> do
    f <- double
    return $ Const $ ValDouble f
  <|> do
    s <- stringLit
    return $ Const $ ValString s
  <|> do
    f <- identifier
    a <- parens $ getFuncArgs
    return $ Fun f a
  <|> do
    v <- identifier
    return $ Var v
  <|> parens expr
  <?> "term"

-- Definicia struktury prikazov jazyka
data Cmd =
      Empty                             -- prazdny prikaz
    | IfStmt Expr Cmd Cmd               -- if
    | WhileStmt Expr Cmd                -- while
    | Func Type String [Param] Cmd      -- definicia funkcie
    | FuncDecl Type String [Param]      -- deklaracia funkcie (bez tela)
    | FuncCall String [Arg]             -- volanie funkcie
    | AssignStmt String Expr            -- priradenie premennej
    | VarDefStmt Type String            -- deklaracia premennej v bloku
    | ReturnStmt Expr                   -- navrat z funkcie
    | Print Expr                        -- vstavana funkcia Print
    | Scan String                       -- vstavana funkcia Scan
    | Seq [Cmd]                         -- zlozeny prikaz

-- Datove typy v jazyku
data Type =
      String
    | Int
    | Double
    deriving Show

-- Struktura parametrov funkcie
data Param = Param Type String
    deriving Show

-- Struktura argumentov predavanych do funkcie
data Arg = Arg Expr
    deriving Show

-- Syntakticka analyza
command =
    do
        semi
        return Empty
    -- (3.2)
    <|> do                              -- typ id ;
        t <- getType
        i <- identifier
        semi
        return $ VarDefStmt t i
    -- (3.3)
    <|> do                              -- return_type id ( params_list ) ;
        t <- getType
        i <- identifier
        params <- parens $ getParams
        semi
        return $ FuncDecl t i params
    -- (3.3)
    <|> do                              -- return_type id ( params_list ) { command_list }
        t <- getType
        i <- identifier
        params <- parens $ getParams
        c <- command
        return $ Func t i params c
    -- (5)
    <|> do                              -- id ( args_list ) ;
        i <- identifier
        a <- parens $ getFuncArgs
        semi
        return $ FuncCall i a
    -- (4)
    <|> do                              -- return expr ;
        reserved "return"
        e <- expr
        semi
        return $ ReturnStmt e
    <|> do                              -- print ( expr ) ;
        reserved "print"
        e <- parens $ expr
        semi
        return $ Print e
    <|> do                              -- scan ( expr ) ;
        reserved "scan"
        i <- identifier
        semi
        return $ Scan i
    <|> do                              -- id = expr ;
        i <- identifier
        reservedOp "="
        e <- expr
        semi
        return $ AssignStmt i e
    <|> do                              -- if ( expr ) { command_list } else { command_list }
        reserved "if"
        b <- parens $ expr
        c1 <- command
        reserved "else"
        c2 <- command
        return $ IfStmt b c1 c2
    <|> do                              -- while ( expr ) { command_list }
        reserved "while"
        b <- parens $ expr
        c <- command
        return $ WhileStmt b c
    <|> do                              -- { command_list }
        seq <- braces $ many command
        return $ Seq seq
    <?> "command"

-- Tabulka premennych

type VarTable = [(String, Value)]

setVar :: VarTable -> String -> Value -> VarTable
setVar [] var val = [(var, val)]
setVar (s@(v,_):ss) var val =
    if v == var
        then (var, val):ss
        else s : setVar ss var val

getVar :: VarTable -> String -> Value
getVar [] v = error $ "Not found: " ++ v
getVar (s@(var, val):ss) v =
    if v == var
        then val
        else getVar ss v




--end Lex.hs
