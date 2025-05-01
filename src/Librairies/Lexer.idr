module Librairies.Lexer

import public Librairies.Text.Lexer.Core

elem : List Char -> Char -> Bool
elem [] c = False
elem (x :: xs) c =
  if x == c
     then True
     else elem xs c

export
any : Recogniser
any = Pred $ const True

export
is : (c : Char) -> Recogniser
is c = pred (==c)

export
isNot : (c : Char) -> Recogniser
isNot c = pred (/=c)

export
like : (c : Char) -> Recogniser
like c = pred (\c1 => toUpper c == toUpper c1)

export
notLike : (c : Char) -> Recogniser
notLike c = pred (\c1 => toUpper c /= toUpper c1)

export
exact : (str : String) -> Recogniser
exact str = 
  case unpack str of
    [] => fail
    xs => concatMap is xs

export
approx : (str : String) -> Recogniser
approx str = 
  case unpack str of
      [] => fail
      xs =>  concatMap like xs

export
oneOf : List Char -> Recogniser
oneOf cs = pred $ elem cs

export
opt : Recogniser -> Recogniser
opt l = l <|> empty

mutual
  export
  some : Recogniser -> Recogniser
  some l = l <+> many l

  export
  many : Recogniser -> Recogniser
  many l = opt (some l)

export
manyUntil : Recogniser -> (stop : Recogniser) -> Recogniser
manyUntil l s = many (reject s <+> l)

export
manyThen : Recogniser -> (after: Recogniser) -> Recogniser
manyThen l after = manyUntil l after <+> after 

export
space : Recogniser
space = oneOf $ unpack " \n"

