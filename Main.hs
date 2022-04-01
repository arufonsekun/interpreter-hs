import Interpreter.AST
import Interpreter.Analyzer

-- step (ADD (NUM 1) (NUM 2))
-- step (ADD (NUM 1) (ADD (NUM 2) (NUM 3)))
-- step (IF FALSE (NUM 1) (NUM 2))
-- step (IF TRUE (NUM 1) (NUM 2))
