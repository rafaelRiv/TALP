module Parser.Parser

import public Parser.Lexer.Source
import public Parser.Rule.Source

export
parse : (Show ty) => String -> Rule ty -> IO ()
parse file prog = do
  let toks = Parser.Lexer.Source.lex file
  putStrLn $ show $ toks
  let ttimp = Librairies.Text.Parser.Core.parse toks  prog
  case ttimp of
      Failure err => putStrLn err
      Success toks ttimp => putStrLn $ show ttimp
  
