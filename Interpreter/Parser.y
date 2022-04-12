{
module Interpreter.Parser where
import Data.Char
import Interpreter.AST
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    true  { TokenTRUE }
    false { TokenFALSE }
    num   { TokenNUM $$ }
    '+'   { TokenADD }
    '&'   { TokenAND }
    if    { TokenIF  }
    '{'   { TokenPairOpening }
    ','   { TokenPairSeparator }
    '}'   { TokenPairClosing }
    '.'   { TokenProjection }
    fst   { TokenFirst }
    snd   { TokenSecond }
%%

Exp  : true { TRUE }
     | false { FALSE }
     | num  { NUM $1 }
     | Exp '+' Exp { ADD $1 $3 }
     | Exp '&' Exp { AND $1 $3 }
     | if Exp Exp Exp { IF $2 $3 $4 }
     | '{' Exp ',' Exp '}' { PAIR $2 $4 }
     | '{' Exp ',' Exp '}' '.' fst { FIRST (PAIR $2 $4) }
     | '{' Exp ',' Exp '}' '.' snd { SECOND (PAIR $2 $4) }

{
parseError :: [Token] -> a
parseError _ = error "Syntax Error: sequência de caracteres inválida."

data Token = TokenTRUE
            | TokenFALSE
            | TokenNUM Int
            | TokenADD
            | TokenAND
            | TokenIF
            | TokenPairOpening
            | TokenPairSeparator
            | TokenPairClosing
            | TokenProjection
            | TokenFirst
            | TokenSecond
            deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c : cs)
    | isSpace c = lexer cs
    | isAlpha c = lexKeyWords (c : cs)
    | isDigit c = lexNum (c : cs)
lexer ('+' : cs) = TokenADD : lexer cs
lexer ('&' : cs) = TokenAND : lexer cs
lexer ('{' : cs) = TokenPairOpening: lexer cs
lexer (',' : cs) = TokenPairSeparator: lexer cs
lexer ('}' : cs) = TokenPairClosing: lexer cs
lexer ('.' : cs) = TokenProjection: lexer cs
lexer _ = error "Lexical error: caracter inválido!"

lexKeyWords cs = case span isAlpha cs of
            ("true", rest) -> TokenTRUE : lexer rest
            ("false", rest) -> TokenFALSE : lexer rest
            ("if", rest) -> TokenIF: lexer rest
            ("fst", rest) -> TokenFirst: lexer rest
            ("snd", rest) -> TokenSecond: lexer rest

lexNum cs = case span isDigit cs of
            (num, rest) -> TokenNUM (read num) : lexer rest
}