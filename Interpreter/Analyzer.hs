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
step (IF TRUE e1 _) = e1
step (IF FALSE _ e2) = e2
step (IF e e1 e2) = case step e of
                     e' -> IF e' e1 e2
-- (E-PairBeta1)
step (FIRST (PAIR (NUM n1) _)) = NUM n1
step (FIRST (PAIR TRUE _)) = TRUE
step (FIRST (PAIR FALSE _)) = FALSE

step (FIRST (PAIR e1 _)) = case step e1 of
                            e1' ->  e1'
-- (E-Proj1)
step (FIRST e) = case step e of
                  e' -> FIRST e'

-- (E-PairBeta2)
step (SECOND (PAIR _ (NUM n1))) = NUM n1
step (SECOND (PAIR _ TRUE)) = TRUE
step (SECOND (PAIR _ FALSE)) = FALSE

step (SECOND (PAIR _ e1)) = case step e1 of
                         e1' ->  e1'
-- (E-Proj2)
step (SECOND e) = case step e of
                  e' -> SECOND e'

-- (E-Pair2)
step (PAIR (NUM n1) e2) = case step e2 of
                           e2' -> PAIR (NUM n1) e2'
step (PAIR FALSE e2) = case step e2 of
                           e2' -> PAIR FALSE e2'
step (PAIR TRUE e2) = case step e2 of
                           e2' -> PAIR TRUE e2'
-- (E-Pair1)
step (PAIR e1 e2) = case step e1 of
                      e1' -> PAIR e1' e2

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
typeof (IF e1 e2 e3) = case typeof e1 of
                        Just T_BOOL -> case (typeof e2, typeof e3) of
                                        (Just T_BOOL, Just T_BOOL) -> Just T_BOOL
                                        (Just T_NUM, Just T_NUM)   -> Just T_NUM
                                        _                          -> Nothing
                        _            -> Nothing

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