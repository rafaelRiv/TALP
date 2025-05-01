{- 
  A lexer combinator take as an in input a String, a list of lexers with tokens and return a list of tokens
-}

module Librairies.Text.Lexer.Core

import Data.Maybe
import Data.List
import Debug.Trace

public export
data Recogniser : Type where
  Empty : Recogniser
  Fail : Recogniser
  Lookahead : Bool -> Recogniser -> Recogniser
  Pred : (Char -> Bool) -> Recogniser
  Seq : Recogniser -> Inf Recogniser -> Recogniser 
  Alt : Recogniser -> Inf Recogniser -> Recogniser

export
empty : Recogniser
empty = Empty

export
fail : Recogniser
fail = Fail

export
reject : Recogniser -> Recogniser
reject r1 = Lookahead False r1

export
pred : (Char -> Bool) -> Recogniser
pred = Pred

export
(<+>) : Recogniser -> Inf Recogniser -> Recogniser
(<+>) = Seq

export
(<|>) : Recogniser -> Inf Recogniser -> Recogniser
(<|>) = Alt

public export
record TokenData a where
  constructor MkTokenData
  line : Int
  col : Int
  lineEnd : Int
  colEnd : Int
  tok : a

public export
Show a => Show (TokenData a) where
  show tokData = "TokenData { line: " ++ show (tokData.line) ++ ", " ++ "col: " ++ show tokData.col  ++ ", lineEnd: " ++ show tokData.lineEnd ++ ", colEnd: " ++ show tokData.colEnd ++ ", tok: " ++ show tokData.tok ++ " }"

public export
TokenMap : (tokenType : Type) -> Type
TokenMap tokenType = List (Recogniser, String -> tokenType)

scan : Recogniser 
    -> (str : List Char)
    -> (tok : List Char)
    -> Maybe (List Char, List Char)
scan Fail _ _ = Nothing
scan Empty str tok = Just (tok,str)
scan (Lookahead positive r1) str tok = 
  if isJust (scan r1 str tok) == positive
     then pure (tok, [])
     else Nothing
scan (Pred _) [] _ = Nothing
scan (Pred pred) (x :: xs) tok =
  if pred x
     then Just ((x::tok),xs)
     else Nothing
scan (Seq r1 r2) str tok =
  do (tok,rest) <- scan r1 str tok
     scan r2 rest tok
scan (Alt r1 r2) str tok =
    maybe (scan r2 str tok) Just (scan r1 str tok)

tokenize : List Char -> TokenMap a -> Int -> Int -> List (TokenData a) -> List (TokenData a)
tokenize str tms line col acc
    = case getFirstToken tms str of
           Just (rest, tok) => tokenize rest tms tok.lineEnd tok.colEnd (tok::acc)
           Nothing => reverse acc
  where
    getLine : List Char -> Int
    getLine str = cast $ length $ filter (=='\n') str

    getCol : List Char -> Int -> Int
    getCol str c = case span (/='\n') (reverse str) of
                    (str,[]) => c + (cast $ length str)
                    (str,_) => cast $ length str

    getFirstToken : TokenMap a -> List Char -> Maybe (List Char, TokenData a)
    getFirstToken [] str = Nothing
    getFirstToken _ [] = Nothing
    getFirstToken ((lex,fn) :: tms) str
      = case scan lex str [] of
          Just (tok, rest) =>
            let endLine = line + getLine tok
                endCol = getCol tok col
            in 
              Just (
                rest,
                MkTokenData line col endLine endCol (fn $ fastPack $ reverse tok)
              )
          Nothing => getFirstToken tms str

export
concatMap : (a -> Recogniser) -> (xs : List a) -> Recogniser
concatMap f [] = Empty
concatMap f (x :: []) = f x
concatMap f (x :: xs) = (f x) <+> concatMap f xs

export
lex : String -> TokenMap a -> List (TokenData a)
lex str xs = tokenize (unpack str) xs 0 0 []


