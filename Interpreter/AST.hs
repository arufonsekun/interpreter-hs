module Interpreter.AST where

data EXPR = TRUE
            | FALSE
            | NUM Int
            | ADD EXPR EXPR
            | AND EXPR EXPR
            | IF  EXPR EXPR EXPR
        deriving Show

data TYPE = T_NUM
          | T_BOOL
          | T_ADD
          | T_AND
          | T_IF
          deriving Show