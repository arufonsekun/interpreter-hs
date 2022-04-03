module Interpreter.AST where

data Expr = TRUE
            | FALSE
            | NUM Int
            | ADD Expr Expr
            | AND Expr Expr
            | IF  Expr Expr Expr
            | PAIR Expr Expr
            | FIRST Expr
            | SECOND Expr
            deriving (Show, Eq)

data TYPE = T_NUM
          | T_BOOL
          | T_PAIR
          deriving Show

-- Construtor viável de adicionar:
-- 1. t1 ; t2 -> expressões em sequência
-- 2. let -> atribuir um nome a uma expressão
-- 3. pairs -> tipo de dados composto
-- 4. record -> {x=5, y=2}, me parece complicado