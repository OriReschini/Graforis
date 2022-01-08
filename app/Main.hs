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

run :: [Char] -> IO ()
run file = do f <- readFile file
              res <- eval (parse (alexScanTokens f))
              case res of
                Left e -> print e
                Right (a,e) -> putStrLn $ "Program ran successfully!"
            
printHelp :: IO ()
printHelp = do putStrLn "Error, no input file."

-- used for debugging
showEnv :: Env -> IO ()
showEnv [] = print ""
showEnv ((n,g):e) = do putStrLn $ n ++ " : " ++ show g
                       showEnv e