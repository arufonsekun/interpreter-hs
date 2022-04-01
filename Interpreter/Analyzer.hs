module Interpreter.Analyzer where
import Interpreter.AST

step :: EXPR -> Maybe EXPR
step (ADD (NUM n1) (NUM n2)) = Just (NUM (n1 + n2))
step (ADD (NUM n1) e2) = case step e2 of
                          Just e2' -> Just (ADD (NUM n1) e2')
                          Nothing -> Nothing
step (ADD e1 e2) = case step e1 of
                    Just e1' -> Just (ADD e1' e2)
                    Nothing -> Nothing
step (AND TRUE e2) = Just e2
step (AND FALSE _) = Just FALSE
step (AND _ FALSE) = Just FALSE
step (AND e1 e2) = case step e1 of
                    Just e1' -> Just (AND e1' e2)
                    Nothing  -> Nothing
step (IF TRUE e1 _) = Just e1
step (IF FALSE _ e2) = Just e2
step (IF e e1 e2) = case step e of
                     Just e' -> Just (IF e' e1 e2)
                     Nothing -> Nothing
step e = Just e

typeof :: EXPR -> Maybe TYPE
typeof TRUE = Just T_BOOL
typeof FALSE = Just T_BOOL
typeof (NUM _) = Just T_NUM
typeof (ADD e1 e2) = case (typeof e1, typeof e2) of
                        (Just T_NUM, Just T_NUM) -> Just T_NUM
                        _              -> Nothing
typeof (AND e1 e2) = case (typeof e1, typeof e2) of
                        (Just T_BOOL, Just T_BOOL) -> Just T_BOOL
                        _                -> Nothing
typeof (IF _ e1 e2) = case (typeof e1, typeof e2) of
                        (Just T_NUM, Just T_NUM)   -> Just T_NUM
                        (Just T_BOOL, Just T_BOOL) -> Just T_BOOL
                        _                          -> Nothing
