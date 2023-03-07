module Main where

import Lexer 
import Parser
import Interpreter
import TypeChecker

main = getContents >>= print . step . typecheck . parser . lexer
