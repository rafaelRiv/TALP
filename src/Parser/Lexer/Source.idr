module Parser.Lexer.Source

import public Librairies.Lexer

public export
data Token =
  Comment String |
  Ident String |
  Symbol String |
  EOF

export
Show Token where
  show (Comment a) = "Comment " ++ show a
  show (Ident a) = "Ident " ++ show a
  show (Symbol a) = "Symbol " ++ show a
  show EOF = "EOF"

integerLit : Recogniser
integerLit = pred isDigit <+> many (pred isDigit)


-- Lets start with only => (implication) and : (type) 
symbol : Recogniser
symbol = oneOf $ fastUnpack "=>:()*+-/" 

ident : Recogniser
ident = pred isAlphaNum <+> many (pred isAlpha)

tokenmaps : TokenMap Token
tokenmaps = [
  (space, \str => Comment str),
  (symbol, \str => Symbol str),
  (ident, \str => Ident str)
]

notComment : Token -> Bool
notComment (Comment x) = False
notComment _ = True

export
lex : String -> List (TokenData Token)
lex str =  (filter (notComment . tok) (lex str tokenmaps)) ++ [MkTokenData 0 0 0 0 EOF]

