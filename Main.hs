import Interpreter.AST
import Interpreter.Analyzer
import Interpreter.Parser

main = getContents >>= print . evaluate . typecheck . parser . lexer 
