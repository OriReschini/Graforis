module Main (main) where

import Lib
import System.Environment (getArgs)
import Lexer
import Parser
import State
import AST 
import Eval               
                    
main :: IO ()
main = do args <- getArgs
          case args of
            (file:_) -> run file
            _ -> printHelp

-- ARREGLAR ESTO
showEnv :: Env -> IO ()
showEnv [] = print ""
showEnv ((n,g):e) = do putStrLn $ n ++ " : " ++ show g
                       showEnv e


run :: [Char] -> IO ()
run file = do f <- readFile file
              print (parse (alexScanTokens f))
              res <- eval (parse (alexScanTokens f))
              print res
              --case eval (parse (alexScanTokens f)) of
              --  Left error -> print error
                --Right (list, env) -> 
                  --drawList list 
                  --showEnv env --asquerosidad
            

printHelp :: IO ()
printHelp = do putStrLn "Error, no input file."
