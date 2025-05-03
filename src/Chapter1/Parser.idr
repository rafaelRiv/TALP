module Chapter1.Parser

import public Parser.Rule.Source
import Chapter1.Lang

boolean : Rule Arith
boolean = 
      (keyword "true" <&> const ATrue)
  <|> (keyword "false" <&> const AFalse)

zero : Rule Arith
zero = keyword "0" <&> const AZero

mutual
  branch : Rule Arith
  branch = do
    keyword "if"
    exp1 <- t
    keyword "then"
    exp2 <- t
    keyword "else"
    exp3 <- t
    pure $ ABranch exp1 exp2 exp3

  succ : Rule Arith
  succ = do
    keyword "succ"
    exp <- t
    pure $ ASucc exp

  pred : Rule Arith
  pred = do
    keyword "pred"
    exp <- t
    pure $ APred exp

  isZero : Rule Arith
  isZero = do
    keyword "isZero"
    exp <- t
    pure $ AIsZero exp

  t : Rule Arith
  t =   boolean 
    <|> branch
    <|> zero
    <|> succ
    <|> pred
    <|> isZero

export
prog : Rule (List Arith)
prog = do
  exp <- t
  next <- prog <|> (eof <&> const [])
  pure (exp::next)


