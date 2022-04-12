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
          | T_PAIR TYPE TYPE
          deriving Show