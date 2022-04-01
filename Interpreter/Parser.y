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
    -- if    { TokenIF  }
%%

Exp  : true { TRUE }
     | false { FALSE }
     | num  { NUM $1 }
     | Exp '+' Exp { ADD $1 $3 }
     | Exp '&' Exp { AND $1 $3 }
    --  | if Exp Exp Exp { IF $1 $2 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Syntax Error: sequência de caracteres inválida."

data Token = TokenTRUE
            | TokenFALSE
            | TokenNUM Int
            | TokenADD
            | TokenAND
            -- | TokenIF
            deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c : cs)
    | isSpace c = lexer cs
    | isAlpha c = lexBool (c : cs)
    | isDigit c = lexNum (c : cs)
lexer ('+' : cs) = TokenADD : lexer cs
lexer ('&' : cs) = TokenAND : lexer cs
lexer _ = error "Lexical error: caracter inválido!"

lexBool cs = case span isAlpha cs of
            ("true", rest) -> TokenTRUE : lexer rest
            ("false", rest) -> TokenFALSE : lexer rest

lexNum cs = case span isDigit cs of
            (num, rest) -> TokenNUM (read num) : lexer rest
}