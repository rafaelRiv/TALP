module Librairies.Text.Parser.Core

export
data Grammar : tok -> (ty : Type) -> Type where
  Empty : ty -> Grammar tok ty
  Terminal : String -> (tok -> Maybe ty) -> Grammar tok ty
  Bind : Grammar tok a -> Inf (a -> Grammar tok b) -> Grammar tok b
  Seq :  Grammar tok a -> Inf (Grammar tok b) -> Grammar tok b
  Alt : Grammar tok ty -> Inf (Grammar tok ty) -> Grammar tok ty

export
Functor (Grammar tok) where
  map f (Empty val) = Empty (f val)
  map f (Terminal err g) = Terminal err (\tok => map f (g tok))
  map f (Bind acc next) = Bind acc (\val => map f (next val))
  map f (Seq acc next) = Seq acc (map f next)
  map f (Alt g g1) = Alt (map f g) (map f g1)

export
(>>=) : Grammar tok a -> Inf (a -> Grammar tok b) -> Grammar tok b
(>>=) = Bind

export
(>>) : Grammar tok a -> Inf (Grammar tok b) -> Grammar tok b
(>>) = Seq

export
pure :  ty -> Grammar tok ty
pure = Empty

export
terminal : String -> (tok -> Maybe ty) -> Grammar tok ty
terminal = Terminal

export
(<|>) : Grammar tok ty -> Inf (Grammar tok ty) -> Grammar tok ty
(<|>) = Alt

public export
data ParseResult : Type -> Type -> Type where
  Failure : String -> ParseResult tok ty
  Success : List tok -> (val: ty) -> ParseResult tok ty

export
parse : Show tok => List tok -> Grammar tok ty -> ParseResult tok ty
parse [] (Empty val) = Success [] val
parse [] (Terminal error _) = Failure error
parse [] (Bind acc next) =
  case parse [] acc of
      Failure err => Failure err
      (Success rest ty) => parse [] (next ty)
parse [] (Seq g1 g2) =
  case parse [] g1 of
      Failure err => Failure err
      (Success rest ty) => parse [] g2
parse [] (Alt g1 g2) =
  case parse [] g1 of
      Failure err => case parse [] g2 of
        Failure err1 => Failure err1
        (Success res ty) => Success [] ty
      (Success res ty) => Success [] ty
parse (tok::toks) (Empty val) = Success (tok::toks) val
parse (tok :: toks) (Terminal err fn) =
  case fn tok of
    Nothing => Failure (err ++ " : " ++ show tok)
    Just ty => Success toks ty
parse (tok::toks) (Bind acc next) =
  case parse (tok::toks) acc of
      Failure err => Failure err
      (Success res ty) => parse res (next ty)
parse (tok::toks) (Seq g1 g2) =
  case parse (tok::toks) g1 of
      Failure err => Failure err
      (Success res ty) => parse res g2
parse (tok::toks) (Alt g1 g2) =
  case parse (tok::toks) g1 of
      Failure err => case parse (tok::toks) g2 of
        Failure err1 => Failure err1
        (Success res ty) => Success res ty
      (Success res ty) => Success res ty



