{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Interpreter.Analyzer where
import Interpreter.AST

step :: Expr -> Expr
step (ADD (NUM n1) (NUM n2)) = NUM (n1 + n2)
step (ADD (NUM n1) e2) = case step e2 of
                          e2' -> ADD (NUM n1) e2'
step (ADD e1 e2) = case step e1 of
                    e1' -> ADD e1' e2
step (AND TRUE e2) = e2
step (AND FALSE _) = FALSE
step (AND _ FALSE) = FALSE
step (AND e1 e2) = case step e1 of
                    e1' -> AND e1' e2
-- step (IF TRUE e1 _) = e1
-- step (IF FALSE _ e2) = e2
-- step (IF e e1 e2) = case step e of
--                      e' -> IF e' e1 e2
step e = e

typeof :: Expr -> Maybe TYPE
typeof TRUE = Just T_BOOL
typeof FALSE = Just T_BOOL
typeof (NUM _) = Just T_NUM
typeof (ADD e1 e2) = case (typeof e1, typeof e2) of
                        (Just T_NUM, Just T_NUM) -> Just T_NUM
                        _              -> Nothing
typeof (AND e1 e2) = case (typeof e1, typeof e2) of
                        (Just T_BOOL, Just T_BOOL) -> Just T_BOOL
                        _                -> Nothing
-- typeof (IF _ e1 e2) = case (typeof e1, typeof e2) of
--                         (Just T_NUM, Just T_NUM)   -> Just T_NUM
--                         (Just T_ADD, Just T_NUM)   -> Just T_ADD
--                         (Just T_NUM, Just T_ADD)   -> Just T_ADD
--                         (Just T_ADD, Just T_ADD)   -> Just T_ADD
--                         (Just T_BOOL, Just T_BOOL) -> Just T_BOOL
--                         (Just T_AND, Just T_BOOL)  -> Just T_AND
--                         (Just T_BOOL, Just T_AND)  -> Just T_AND
--                         (Just T_AND, Just T_AND)  -> Just T_AND
--                         _                          -> Nothing

-- Professor's typeof
-- typeof :: Expr -> Maybe TYPE
-- typeof TRUE = Just T_BOOL
-- typeof FALSE = Just T_BOOL
-- typeof (NUM _) = Just T_NUM
-- typeof (ADD e1 e2) = case typeof e1 of
--                        Just T_NUM -> case typeof e2 of
--                            Just T_NUM -> Just T_NUM
--                            _          -> Nothing
--                        _              -> Nothing
-- typeof (AND e1 e2) = case (typeof e1, typeof e2) of
--                        (Just T_BOOL, Just T_BOOL) -> Just T_BOOL
--                        _                     -> Nothing
-- typeof (IF _ e1 e2) = case (typeof e1, typeof e2) of
--                             (Just T_NUM, Just T_NUM) -> Just T_NUM
--                             (Just T_BOOL, Just T_BOOL) -> Just T_BOOL
--                             _     -> Nothing

evaluate :: Expr -> Expr
evaluate e = case step e of
                e' -> if e' == e
                        then e
                        else evaluate e'
                _ -> error "Semantic error: erro avaliando a expressão"
 

typecheck :: Expr -> Expr
typecheck e = case typeof e of 
                Just _ -> e
                _ -> error "Type error: erro na verificação de tipos."