module Main( main ) where

import Lex
import System.Environment( getArgs )
import System.IO

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

aep = do
    whiteSpace
    --ast <- cmd
    eof
    --return ast
    return ()
    <?> "aep"

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
        let ast = parseAep input fileName
        return ()
        --interpret [] ast 
