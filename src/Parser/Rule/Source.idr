module Parser.Rule.Source

import public Librairies.Parser
import public Parser.Lexer.Source

public export
Rule : Type -> Type
Rule ty = Grammar (TokenData Token) ty

export
keyword : String -> Rule ()
keyword req = 
    terminal 
      ("Expect keyword : " ++ req)
      (\tok => case tok.tok of 
                  Ident i => if i == req then Just () else Nothing
                  _ => Nothing)

export
symbol : String -> Rule ()
symbol req = 
    terminal 
      ("Expect symbol : " ++ req)
      (\tok => case tok.tok of 
                  Symbol s => if s == req then Just () else Nothing
                  _ => Nothing)

export
ident : Rule String
ident = 
    terminal 
      "Expect identifier"
      (\tok => case tok.tok of 
                  Ident i => Just i
                  _ => Nothing)

      


