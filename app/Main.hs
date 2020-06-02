module Main where

import Lib
import System.Environment (getArgs)
import Lexer
import Parser

main :: IO ()
--main = someFunc

main = do args <- getArgs
          case args of
            [] -> putStrLn "Error: there is no input file."
            (file:_) -> run file

run :: [Char] -> IO ()
run file = do f <- readFile file
              print (alexScanTokens f)
              print (parse (alexScanTokens f))
