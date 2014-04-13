module Main( main ) where

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
    , reservedNames  = [ "double", "else", "if", "int", "print", "scan", "string", "while", "return" ]
    , caseSensitive  = True
    }

-- Lexikalny analyzator
lexer = P.makeTokenParser aelDef

-- Pomocne funkcie lexikalnej analyzy
whiteSpace = P.whiteSpace lexer
integer    = P.integer lexer
intOrFloat = P.naturalOrFloat lexer
stringLit  = P.stringLiteral lexer
parens     = P.parens lexer
braces     = P.braces lexer
semi       = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
comma      = P.comma lexer

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
  deriving (Show, Eq)

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
getMoreParams t i = do
        comma
        p <- getNoEmptyParams
        return $ (Param t i):p
    <|> do
        return $ [(Param t i)]

getParams = do
        t <- getType
        i <- identifier
        getMoreParams t i
    <|> do
        return []

getNoEmptyParams = do
        t <- getType
        i <- identifier
        getMoreParams t i

-- Ziskanie argumentov funkcie pri jej volani
getMoreFuncArgs e = do
        comma
        a <- getNoEmptyFuncArgs
        return $ (Arg e):a
    <|> do
        return $ [(Arg e)]


getFuncArgs = do
        e <- expr
        getMoreFuncArgs e
    <|> do
        return []

getNoEmptyFuncArgs = do
        e <- expr
        getMoreFuncArgs e

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
term_ident i =
    do
        a <- parens $ getFuncArgs
        return $ Fun i a
    <|> do
        return $ Var i

term = do
    f <- intOrFloat
    case f of 
    	Left i -> return $ Const $ ValInt $ fromInteger i
    	Right d -> return $ Const $ ValDouble d    
  <|> do
    s <- stringLit
    return $ Const $ ValString s
  <|> do
    i <- identifier
    term_ident i
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
    deriving (Show, Eq)

-- Datove typy v jazyku
data Type =
      String
    | Int
    | Double    
    deriving (Show, Eq)

-- Struktura parametrov funkcie
data Param = Param Type String
    deriving (Show, Eq)

-- Struktura argumentov predavanych do funkcie
data Arg = Arg Expr
    deriving (Show, Eq)

-- Syntakticka analyza

command_ident i =
    do                                  -- id ( args_list )
        a <- parens $ getFuncArgs
        semi
        return $ FuncCall i a 
    <|> do                              -- id = expr ;
        reservedOp "="
        e <- expr
        semi
        return $ AssignStmt i e

command_local_body = 
    do                                  -- id ( args_list ) ; id = expr ;
        i <- identifier
        command_ident i
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
        i <- parens $ identifier
        semi
        return $ Scan i
    <|> do                              -- if ( expr ) { command_list } else { command_list }
        reserved "if"
        b <- parens $ expr
        c1 <- command_local_body
        reserved "else"
        c2 <- command_local_body
        return $ IfStmt b c1 c2
    <|> do                              -- while ( expr ) { command_list }
        reserved "while"
        b <- parens $ expr
        c <- command_local_body
        return $ WhileStmt b c
    <|> do                              -- { command_list }
        seq <- braces $ many command_local_body
        return $ Seq seq

command_local_decl = 
    do
        semi
        return Empty
    <|> do                              -- typ id ;
        t <- getType
        i <- identifier
        semi
        return $ VarDefStmt t i
        
command_local_tog = 
    do
        seq_dec <- many command_local_decl
        seq_body <- many command_local_body
        return $ seq_dec ++ seq_body
        
command_local = 
    do                                  -- { command_list }
        seq <- braces $ command_local_tog
        return $ Seq seq

command_func t i p =
    do                                  -- return_type id ( params_list ) ;
        semi
        return $ FuncDecl t i p
    <|> do                              -- return_type id ( params_list ) { command_list }
        c <- command_local
        return $ Func t i p c

command_global_ident t i =
    do
        semi
        return $ VarDefStmt t i
    <|> do
        params <- parens getParams
        command_func t i params

command_global_decl =
    do
        semi
        return Empty
    <|> do                              -- typ id ;
        t <- getType
        i <- identifier
        command_global_ident t i

command_global =
    do
        seq_global <- many command_global_decl
        return $ seq_global

command =
    do                                  -- program
        seq <- braces $ command_global
        return $ Seq seq
    <?> "command"
        
        
        
-- Tabulka premennych
type VarTable = [(String, Value)]

setVar :: VarTable -> String -> Value -> VarTable
setVar [] var val = [(var, val)]
setVar (s@(v,vT):ss) var val =
    if v == var
        then typeCorrect vT val 
        else s : setVar ss var val
    where -- overovanie typov pri priradeni hodnoty do premennej.
        typeCorrect (ValInt i1) (ValInt i2) = (var, val):ss
        typeCorrect (ValDouble i1) (ValInt i2) = (var, ValDouble $ fromIntegral i2):ss
        typeCorrect (ValDouble i1) (ValDouble i2) = (var, val):ss
        typeCorrect (ValString i1) (ValString i2) = (var, val):ss
        typeCorrect _ _ = error $ "Type missmatch in setting variable :" ++ var 
            

getVar :: VarTable -> String -> Value
getVar [] v = error $ "Variable not found in symbol table: " ++ v
getVar (s@(var, val):ss) v =
    if v == var
        then val
        else getVar ss v
        
isVar :: VarTable -> String -> Bool
isVar [] n = False
isVar ((name, val):vs) n
    | name == n = True
    | otherwise = isVar vs n

-- Tabulka funkcii
data FuncRecord =   FuncRecord
                    { funcName      :: String
                    , funcType      :: Type
                    , funcParams    :: [Param]
                    , funcCommands  :: Cmd
                    } deriving Show

type FuncTable = [FuncRecord]

originalParametersTest :: [Param] -> [String] -> [Param]
originalParametersTest [] _ = []
originalParametersTest ((Param ptype pname):ps) ls = if isInList pname ls
                                                 then error "Multiple parameters with same name!"
                                                 else ((Param ptype pname):originalParametersTest ps (pname:ls))
    
setFunc :: FuncTable -> String -> Type -> [Param] -> Cmd -> Bool -> FuncTable
setFunc [] n t ps c b       -- if Bool (before main) is True, then save, else dont save
    | b == True = [FuncRecord {funcName=n, funcType=t, funcParams=originalParametersTest ps [], funcCommands=c}]
    | otherwise = []
setFunc ft@(f:fs) n t ps c b
    | funcName f == n = updateFunc f t (originalParametersTest ps []) c
    | otherwise = f : setFunc fs n t ps c b
    where
        updateFunc f t ps c = 
            if funcType f == t
            then
                if funcParams f == ps
                then
                    if funcCommands f == Empty
                    then
                        if c /= Empty
                        then
                            FuncRecord {funcName=n, funcType=t, funcParams=ps, funcCommands=c} : fs
                        else
                          error $ "Multiple declarations of function: " ++ funcName f  
                    else
                      error $ "Multiple definitions of function: " ++ funcName f  
                else
                  error $ "Multiple declarations of function with different parameters: " ++ funcName f  
            else
                error $ "Multiple declarations of function with different types: " ++ funcName f

getFuncType :: FuncTable -> String -> Type
getFuncType [] fName = error $ "Cannot access type of function: " ++ fName
getFuncType (f:fs) fName
    | funcName f == fName = funcType f
    | otherwise = getFuncType fs fName

getFuncResult :: SymTable -> FuncTable -> String -> [Arg] -> IO SymTable
getFuncResult _ [] fName _ = error $ "Undefined function call: " ++ fName
getFuncResult st@(gt, ft, lt, gc) (f:fs) fName fArgs
    | funcName f == fName = do
            ltTable <- assignArgsToParams st [] fName (funcParams f) fArgs
            interpret (gt, ft, ltTable, gc) (funcCommands f)
    | otherwise = getFuncResult st fs fName fArgs

assignArgsToParams :: SymTable -> VarTable -> String -> [Param] -> [Arg] -> IO VarTable
assignArgsToParams _ vt _ [] [] = return vt
assignArgsToParams _ _ n (p:ps) [] = error $ "Function called with less arguments than required: " ++ n
assignArgsToParams _ _ n [] (arg:args) = error $ "Function called with more arguments than required: " ++ n
assignArgsToParams st vt n ((Param pType pName):ps) ((Arg ex):args) = do
    evaluated <- eval st ex
    case (pType, evaluated) of
        (Int, ValInt val)       -> assignArgsToParams st (setVar vt pName $ ValInt val) n ps args
        (Double, ValInt val)    -> assignArgsToParams st (setVar vt pName $ ValDouble $ fromIntegral val) n ps args
        (Double, ValDouble val) -> assignArgsToParams st (setVar vt pName $ ValDouble val) n ps args
        (String, ValString val) -> assignArgsToParams st (setVar vt pName $ ValString val) n ps args
        _   -> error $ "Passing parameter of incompatible type to a function: " ++ n

-- Tabulka symbol
type SymTable = (VarTable, FuncTable, VarTable, Bool)   -- Globalne premenne, Funkcie, Lokalne premenne, [Globalny kontext == True, Lokalny kontext == false]

getSym :: SymTable -> String -> Value
getSym  (gt, ft, lt, gc) name
    | isLocal name = getVar lt name
    | otherwise = getVar gt name
    where
        isLocal n = isVar lt n

addSym :: SymTable -> String -> Value -> SymTable
addSym (gt, ft, lt, gc) vName val
    | gc == True && not (isVar gt vName) = (setImplicitSym gt, ft, lt, gc)
    | gc == False && not (isVar lt vName) = (gt, ft, setImplicitSym lt, gc)
    | otherwise = error $ "Multiple declarations of variable: " ++ vName
    where
        setImplicitSym t  = setVar t vName val

setSym :: SymTable -> String -> Value -> SymTable
setSym (gt, ft, lt, gc) vName val
    | gc == False && (isVar lt vName) = (gt, ft, newVt lt, gc)
    | isVar gt vName = (newVt gt, ft, lt, gc)
    | otherwise = error $ "Cannot assign to non-existing variable: " ++ vName
    where
        newVt t = setVar t vName val

getFun :: SymTable -> String -> [Arg] -> IO SymTable
getFun st@(gt, ft, lt, gc) n args = do
    getFuncResult st ft n args

isFun :: FuncTable -> String -> Bool
isFun [] name = False
isFun (f:ft) name 
	| name == funcName f = True
	| otherwise = isFun ft name

setFun :: SymTable -> String -> Type -> [Param] -> Cmd -> SymTable
setFun (gt, ft, lt, gc) n t ps c = (gt, newFt, lt, gc)
    where
        newFt = setFunc ft n t ps c gc
        
setGCon :: SymTable -> SymTable
setGCon (gt, ft, lt, gc) = (gt, ft, lt, True)

setLCon :: SymTable -> SymTable
setLCon (gt, ft, lt, gc) = (gt, ft, lt, False)

switchCon :: SymTable -> SymTable
switchCon (gt, ft, lt, gc) = (gt, ft, lt, not gc)

-- Vyhodnotenie vyrazov
eval :: SymTable -> Expr -> IO Value
eval ts (Const i) = return i

eval ts (Add e1 e2) = do
    evalLeft <- eval ts e1
    evalRight <- eval ts e2
    ev evalLeft evalRight
	where 
		ev (ValInt i1) (ValInt i2) = return $ ValInt (i1 + i2)
		ev (ValInt i1) (ValDouble i2) = return $ ValDouble (fromIntegral i1 + i2)
		ev (ValDouble i1) (ValInt i2) = return $ ValDouble (i1 + fromIntegral i2)
		ev (ValDouble i1) (ValDouble i2) = return $ ValDouble (i1 + i2)
		ev (ValString i1) (ValString i2) = return $ ValString (i1 ++ i2)
		ev _ _ = error "Type missmatch in operator +"

eval ts (Sub e1 e2) = do
    evalLeft <- eval ts e1
    evalRight <- eval ts e2
    ev evalLeft evalRight
	where 
		ev (ValInt i1) (ValInt i2) = return $ ValInt (i1 - i2)
		ev (ValInt i1) (ValDouble i2) = return $ ValDouble (fromIntegral i1 - i2)
		ev (ValDouble i1) (ValInt i2) = return $ ValDouble (i1 - fromIntegral i2)
		ev (ValDouble i1) (ValDouble i2) = return $ ValDouble (i1 - i2)		
		ev _ _ = error "Type missmatch in operator -"

eval ts (Mult e1 e2) = do
    evalLeft <- eval ts e1
    evalRight <- eval ts e2
    ev evalLeft evalRight
	where 
		ev (ValInt i1) (ValInt i2) = return $ ValInt (i1 * i2)
		ev (ValInt i1) (ValDouble i2) = return $ ValDouble (fromIntegral i1 * i2)
		ev (ValDouble i1) (ValInt i2) = return $ ValDouble (i1 * fromIntegral i2)
		ev (ValDouble i1) (ValDouble i2) = return $ ValDouble (i1 * i2)		
		ev _ _ = error "Type missmatch in operator *"

eval ts (Div e1 e2) = do
    evalLeft <- eval ts e1
    evalRight <- eval ts e2
    ev evalLeft evalRight
	where 
		ev (ValInt i1) (ValInt i2) = if (i2 == 0) then error "Division by zero!" else return $ ValInt (i1 `quot` i2)							
		ev (ValInt i1) (ValDouble i2) = return $ ValDouble (fromIntegral i1 / i2)
		ev (ValDouble i1) (ValInt i2) = return $ ValDouble (i1 / fromIntegral i2)
		ev (ValDouble i1) (ValDouble i2) = return $ ValDouble (i1 / i2)		
		ev _ _ = error "Type missmatch in operator /"

eval ts (Gt e1 e2) = do
    evalLeft <- eval ts e1
    evalRight <- eval ts e2
    ev evalLeft evalRight
	where 
		ev (ValInt i1) (ValInt i2) =  if (i1 > i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev (ValDouble i1) (ValDouble i2) = if (i1 > i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev (ValString i1) (ValString i2) = if (i1 > i2) then  return $(ValInt 1) else return $ (ValInt 0)
		ev _ _ = error "Type missmatch in operator >"

eval ts (GtEq e1 e2) = do
    evalLeft <- eval ts e1
    evalRight <- eval ts e2
    ev evalLeft evalRight
	where 
		ev (ValInt i1) (ValInt i2) =  if (i1 >= i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev (ValDouble i1) (ValDouble i2) = if (i1 >= i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev (ValString i1) (ValString i2) = if (i1 >= i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev _ _ = error "Type missmatch in operator >="

eval ts (Lt e1 e2) = do
    evalLeft <- eval ts e1
    evalRight <- eval ts e2
    ev evalLeft evalRight
	where 
		ev (ValInt i1) (ValInt i2) =  if (i1 < i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev (ValDouble i1) (ValDouble i2) = if (i1 < i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev (ValString i1) (ValString i2) = if (i1 < i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev _ _ = error "Type missmatch in operator <"

eval ts (LtEq e1 e2) = do
    evalLeft <- eval ts e1
    evalRight <- eval ts e2
    ev evalLeft evalRight
	where 
		ev (ValInt i1) (ValInt i2) =  if (i1 <= i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev (ValDouble i1) (ValDouble i2) = if (i1 <= i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev (ValString i1) (ValString i2) = if (i1 <= i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev _ _ = error "Type missmatch in operator <="

eval ts (Eq e1 e2) = do
    evalLeft <- eval ts e1
    evalRight <- eval ts e2
    ev evalLeft evalRight
	where 
		ev (ValInt i1) (ValInt i2) =  if (i1 == i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev (ValDouble i1) (ValDouble i2) = if (i1 == i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev (ValString i1) (ValString i2) = if (i1 == i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev _ _ = error "Type missmatch in operator =="

eval ts (Neq e1 e2) = do
    evalLeft <- eval ts e1
    evalRight <- eval ts e2
    ev evalLeft evalRight
	where 
		ev (ValInt i1) (ValInt i2) =  if (i1 /= i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev (ValDouble i1) (ValDouble i2) = if (i1 /= i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev (ValString i1) (ValString i2) = if (i1 /= i2) then return $ (ValInt 1) else return $ (ValInt 0)
		ev _ _ = error "Type missmatch in operator !="

eval ts (Var v) = return $ getSym ts v

eval ts@(gt, ft, lt, gc) (Fun name args) = do
    (gt', ft', lt', gc') <- getFun (gt, ft, lt, gc) name args
    if (isVar lt' "return") then do
	    case (getVar lt' "return", getFuncType ft' name) of
	        (ValInt i, Int) -> return $ ValInt i
	        (ValInt i, Double) -> return $ ValDouble $ fromIntegral i
	        (ValDouble d, Double) -> return $ ValDouble d
	        (ValString s, String) -> return $ ValString s
	        (_,_) -> error $ "Bad type of returned value in function: " ++ name
	else do
		case (getFuncType ft' name) of
			(Int) -> return $ ValInt 0
			(Double) -> return $ ValDouble 0.0
			(String) -> return $ ValString ""

-- Interpret

interpret :: SymTable -> Cmd -> IO SymTable
interpret ts (Empty) = return ts

interpret ts (VarDefStmt t varName) = case t of
        (Int) -> return $ addSym ts varName $ ValInt 0 
        (Double) -> return $ addSym ts varName $ ValDouble 0.0
        (String) -> return $ addSym ts varName $ ValString ""
        
interpret ts (AssignStmt v e) =  do 
    evaluated <- eval ts e 
    return $ setSym ts v evaluated

interpret ts (Print e) = do 
    evaluated <- eval ts e
    case evaluated of
        (ValInt i) -> putStrLn $ show i
        (ValDouble d) -> putStrLn $ show d
        (ValString s) -> putStrLn s
    return ts

interpret ts (Scan var) = do 
    case getSym ts var of         
        (ValInt i) -> do
            readVal <- readLn :: IO Int  
            return $ setSym ts var $ ValInt readVal
        (ValDouble d) -> do
            readVal <- readLn :: IO Double  
            return $ setSym ts var $ ValDouble readVal
        (ValString s) -> do
            readVal <- getLine  
            return $ setSym ts var $ ValString readVal

interpret ts (IfStmt cond cmdTrue cmdFalse) = do
    evaluated <- eval ts cond
    case evaluated of
        (ValInt i) -> if i /= 0 
            then do
                interpret ts cmdTrue
            else do
                interpret ts cmdFalse
        _ -> error "Condition in if statement is not an integer value!"

interpret ts (WhileStmt cond cmd) = do
    evaluated <- eval ts cond
    case evaluated of
        (ValInt i) -> if i /= 0
            then do
                ts' <- interpret ts cmd
                interpret ts' $ WhileStmt cond cmd
            else do
                return ts
        _ -> error "Condition in while statement is not an integer value!"

interpret ts (ReturnStmt e) = do
    evaluated <- eval ts e
    return $ addSym ts "return" evaluated

interpret ts (Seq []) = return ts
interpret ts (Seq (c:cs)) = do
    ts' <- interpret ts c
    case c of
        (ReturnStmt _) -> return ts'
        _ -> interpret ts' $ Seq cs

interpret ts (FuncCall name args) = do
    getFun ts name args

interpret ts (FuncDecl retType funcName params) = return ts

interpret ts (Func retType "main" params cmd) = 
    if (retType == Int && params == []) then do
            let ts' = setLCon ts
            tsAft@(_,_,lt,_) <- interpret ts' cmd
            if (isVar lt "return") then do
            	case (getVar lt "return") of
            		(ValInt i) -> return tsAft
            		_ -> error "Bad type of returning value from main!"
            else do
            	return tsAft
        else do
            error "Main has bad return type or has some parameters!"

interpret ts (Func retType funcName params cmd) =   
    return ts

preInterpret :: SymTable -> Cmd -> IO SymTable
preInterpret ts (VarDefStmt t varName) = return ts
preInterpret ts@(_,_,_,gc) (Func retType "main" params cmd) = return $ setLCon $ setFun ts "main" retType params cmd
preInterpret ts@(_,_,_,gc) (FuncDecl retType "main" params) = return $ setLCon $ setFun ts "main" retType params Empty
preInterpret ts@(_,_,_,gc) (Func retType funcName params cmd) = return $ setFun ts funcName retType params cmd
preInterpret ts@(_,_,_,gc) (FuncDecl retType funcName params) = return $ setFun ts funcName retType params Empty
preInterpret ts (Seq []) = return ts
preInterpret ts (Seq (c:cs)) = do
    ts' <- preInterpret ts c
    preInterpret ts' $ Seq cs
preInterpret ts _ = error "Only declaration or definition can be in global context!"

sectionsTest :: Cmd -> Bool -> Bool
sectionsTest (Seq []) _ = True
sectionsTest (Seq (c:cs)) b = case(c) of
    (VarDefStmt _ _) -> if(b)
                        then sectionsTest (Seq cs) True
                        else False
    _ -> sectionsTest (Seq cs) False

isInList :: String -> [String] -> Bool
isInList _ [] = False
isInList name (l:ls)
    | name == l = True
    | otherwise = isInList name ls

exprDeclDefTest :: Expr -> [String] -> Bool
exprDeclDefTest e lt = case (e) of
    (Fun name args) -> ((isInList name lt) && (argsDeclDefTest args lt))
    (Add e1 e2) -> ((exprDeclDefTest e1 lt) && (exprDeclDefTest e2 lt))
    (Sub e1 e2) -> ((exprDeclDefTest e1 lt) && (exprDeclDefTest e2 lt))
    (Mult e1 e2) -> ((exprDeclDefTest e1 lt) && (exprDeclDefTest e2 lt))
    (Div e1 e2) -> ((exprDeclDefTest e1 lt) && (exprDeclDefTest e2 lt))
    (Gt e1 e2) -> ((exprDeclDefTest e1 lt) && (exprDeclDefTest e2 lt))
    (GtEq e1 e2) -> ((exprDeclDefTest e1 lt) && (exprDeclDefTest e2 lt))
    (Lt e1 e2) -> ((exprDeclDefTest e1 lt) && (exprDeclDefTest e2 lt))
    (LtEq e1 e2) -> ((exprDeclDefTest e1 lt) && (exprDeclDefTest e2 lt))
    (Eq e1 e2) -> ((exprDeclDefTest e1 lt) && (exprDeclDefTest e2 lt))
    (Neq e1 e2) -> ((exprDeclDefTest e1 lt) && (exprDeclDefTest e2 lt))
    _ -> True

argsDeclDefTest :: [Arg] -> [String] -> Bool
argsDeclDefTest [] _ = True
argsDeclDefTest ((Arg e):as) lt = if(exprDeclDefTest e lt)
                                    then argsDeclDefTest as lt
                                    else False

funcDeclDefTest :: Cmd -> [String] -> Bool
funcDeclDefTest (Seq []) _ = True
funcDeclDefTest (Seq (c:cs)) lt = case (c) of
    (FuncDecl _ name _) -> funcDeclDefTest (Seq cs) (name:lt)
    (Func _ name _ cmd) -> if(funcDeclDefTest cmd (name:lt))
                            then funcDeclDefTest (Seq cs) (name:lt)
                            else False
    (FuncCall name args) -> if((isInList name lt) && (argsDeclDefTest args lt))
                            then funcDeclDefTest (Seq cs) lt
                            else False
    (IfStmt e cmd1 cmd2) -> if((exprDeclDefTest e lt) && (funcDeclDefTest cmd1 lt) && (funcDeclDefTest cmd2 lt))
                            then funcDeclDefTest (Seq cs) lt
                            else False
    (WhileStmt e cmd) -> if((exprDeclDefTest e lt) && (funcDeclDefTest cmd lt))
                            then funcDeclDefTest (Seq cs) lt
                            else False
    (AssignStmt _ e) -> if(exprDeclDefTest e lt)
                        then funcDeclDefTest (Seq cs) lt
                        else False
    (ReturnStmt e) -> if(exprDeclDefTest e lt)
                        then funcDeclDefTest (Seq cs) lt
                        else False
    (Print e) -> if(exprDeclDefTest e lt)
                    then funcDeclDefTest (Seq cs) lt
                    else False
    _ -> funcDeclDefTest (Seq cs) lt

aep = do
    whiteSpace
    ast <- command
    eof
    return ast
    <?> "Aep parsing error"

parseAep input file =
    case parse aep file input of
        Left e -> error $ show e
        Right ast -> ast 
    
main = do
    args <- getArgs
    if length args /= 1
    then error "Specify one input file."
    else do
        let fileName = args!!0
        input <- readFile fileName
        let ast = parseAep ("{" ++ input ++ "}") fileName

        if(sectionsTest ast True)
            then do
                if(funcDeclDefTest ast [])
                    then do
                        (_, ft, _, _) <- preInterpret ([],[],[], True) ast        
                        if (isFun ft "main") then do
                	       interpret ([], ft, [], True) ast 
                        else do
                	       error "Missing main function!"
                    else do
                        error "Calling undefined or undeclared function!"
            else do
                error "Global variable declaration after function declaration or definition!"

--end Lex.hs
