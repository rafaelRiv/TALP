module Parser.Lexer.Source

import public Librairies.Lexer

public export
data Token =
  Comment String |
  IntegerLit Int |
  Ident String |
  Symbol String

export
Show Token where
  show (Comment a) = "Comment " ++ show a
  show (IntegerLit a) = "Integer " ++ show a
  show (Ident a) = "Ident " ++ show a
  show (Symbol a) = "Symbol " ++ show a

integerLit : Recogniser
integerLit = pred isDigit <+> many (pred isDigit)


-- Lets start with only => (implication) and : (type) 
symbol : Recogniser
symbol = oneOf $ fastUnpack "=>:()*+-/" 

ident : Recogniser
ident = pred isAlpha <+> many (pred isAlpha)

tokenmaps : TokenMap Token
tokenmaps = [
  (space, \str => Comment str),
  (integerLit, \str => IntegerLit $ cast str),
  (symbol, \str => Symbol str),
  (ident, \str => Ident str)
]

notComment : Token -> Bool
notComment (Comment x) = False
notComment _ = True

export
lex : String -> List (TokenData Token)
lex str =  filter (notComment . tok) (lex str tokenmaps)

