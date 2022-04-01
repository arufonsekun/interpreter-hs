module Interpreter.AST where

data Expr = TRUE
            | FALSE
            | NUM Int
            | ADD Expr Expr
            | AND Expr Expr
            deriving (Show, Eq)
            -- | IF  Expr Expr Expr

data TYPE = T_NUM
          | T_BOOL
          deriving Show

-- Construtor viável de adicionar:
-- 1. t1 ; t2 -> expressões em sequência
-- 2. let -> atribuir um nome a uma expressão
-- 3. pairs -> tipo de dados composto
-- 4. record -> {x=5, y=2}, me parece complicado