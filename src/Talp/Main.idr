module Talp.Main

import Parser.Parser
import Chapter1.Parser as Ch1

import Data.List
import System.File
import System

main : IO ()
main = do
  [_,fname] <- getArgs
      | _ => putStrLn "Usage : tinyidris2 filename"
  putStrLn "Welcome TinyIdris2"
  Right file <- readFile fname
    | Left err => putStrLn $ show err
  parse file Ch1.prog
