module Chapter1.Lang

public export
data Arith : Type where
  ATrue : Arith
  AFalse : Arith
  ABranch : Arith -> Arith -> Arith -> Arith
  AZero : Arith
  ASucc : Arith -> Arith
  APred : Arith -> Arith
  AIsZero : Arith -> Arith

public export
Show Arith where
  show ATrue = "true"
  show AFalse = "false"
  show (ABranch con t1 t2) = "if "  ++ show con ++ " than " ++ show t1 ++ " else " ++ show t2
  show AZero = "zero"
  show (ASucc t) = "succ " ++ show t
  show (APred t) = "pred " ++ show t
  show (AIsZero t) = "isZero " ++ show t 
  
