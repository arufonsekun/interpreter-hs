module Interpreter.Analyzer where
import Interpreter.AST

step :: EXPR -> EXPR
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
step e = e

typeof :: EXPR -> Maybe TYPE
typeof TRUE = Just T_BOOL
typeof FALSE = Just T_BOOL
typeof (NUM _) = Just T_NUM
typeof (ADD (NUM n1) e1) = Just T_ADD
typeof (ADD e1 (NUM n1)) = Just T_ADD
typeof (ADD e1 e2) = case (typeof e1, typeof e2) of
                        (Just T_NUM, Just T_NUM) -> Just T_NUM
                        (Just T_NUM, Just T_ADD) -> Just T_ADD
                        (Just T_ADD, Just T_NUM) -> Just T_ADD
                        (Just T_ADD, Just T_ADD) -> Just T_ADD
                        _              -> Nothing
typeof (AND TRUE e1) = Just T_AND
typeof (AND e1 TRUE) = Just T_AND
typeof (AND FALSE e1) = Just T_AND
typeof (AND e1 FALSE) = Just T_AND
typeof (AND e1 e2) = case (typeof e1, typeof e2) of
                        (Just T_BOOL, Just T_BOOL) -> Just T_BOOL
                        (Just T_AND, Just T_BOOL)  -> Just T_AND
                        (Just T_BOOL, Just T_AND)  -> Just T_AND
                        (Just T_AND, Just T_AND)  -> Just T_AND
                        _                -> Nothing
typeof (IF _ e1 e2) = case (typeof e1, typeof e2) of
                        (Just T_NUM, Just T_NUM)   -> Just T_NUM
                        (Just T_ADD, Just T_NUM)   -> Just T_ADD
                        (Just T_NUM, Just T_ADD)   -> Just T_ADD
                        (Just T_ADD, Just T_ADD)   -> Just T_ADD
                        (Just T_BOOL, Just T_BOOL) -> Just T_BOOL
                        (Just T_AND, Just T_BOOL)  -> Just T_AND
                        (Just T_BOOL, Just T_AND)  -> Just T_AND
                        (Just T_AND, Just T_AND)  -> Just T_AND
                        _                          -> Nothing

-- Professor's typeof
-- typeof :: EXPR -> Maybe TYPE
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

evaluate :: EXPR -> Maybe EXPR
evaluate TRUE = Just TRUE
evaluate FALSE = Just FALSE
evaluate (NUM e) = Just (NUM e)
evaluate exp = case typeof exp of
                 Just T_ADD  -> evaluate(step exp)
                 Just T_AND  -> evaluate(step exp)
                 Just T_IF   -> evaluate(step exp)
                 Just T_BOOL -> Just exp
                 Just T_NUM  -> Just exp
                 _           -> Nothing