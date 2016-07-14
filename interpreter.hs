module interpreter where

data Type = INT | CHAR | FLOAT | DOUBLE | POINTER Type | Rec String [Type] deriving (Eq,Show)

data Expr =
    Var String
  | Value Type
  | Op Expr BinOp Expr
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = Add
  | Sub
  | Mul
  | Div
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement = Assign String Expr
  | IfThen Expr Statement
  | IfThenElse Expr Statement Statement
  | While Expr Statement
  | For Statement Expr Statement Statement
  | Sequence Statement Statement
  | Skip
  deriving (Show, Eq)


type State = String -> Int

extend :: State -> String -> Int -> State
extend  x y z w = if y == w
  then z
  else x w

empty :: State
empty _ = 0

eval :: State -> Expr -> Int
eval st (Var s) = st s
eval _ (Val n) = n
eval st (Op ex1 bo ex2) = case bo of
  Add -> x + y
  Sub -> x - y
  Mul -> x * y
  Div -> x `div` y
  Gt -> boolToInt $ x > y
  Ge -> boolToInt $ x >= y
  Lt -> boolToInt $ x < y
  Le -> boolToInt $ x <= y
  Eql -> boolToInt $ x == y
  where
    x = eval st ex1
    y = eval st ex2
    boolToInt True = 1
    boolToInt False = 0

data DStatement = DAssign String Expr
                | DIfThen Expr DStatement
                | DIfThenElse Expr DStatement DStatement
                | DWhile Expr DStatement
                | DSequence DStatement DStatement
                | DSkip
                deriving (Show, Eq)

desugar :: Statement -> DStatement
desugar (Assign str ex) = DAssign str ex
desugar (Incr str) = DAssign str (Op (Var str) Add (Val 1))
desugar (IfThen ex stat) = DIfThen ex  (desugar stat)
desugar (IfThenElse ex stat stat1) = DIfThenElse ex  (desugar stat) (desugar stat1)
desugar (While ex stat) = DWhile ex (desugar stat)
desugar (For stat ex stat1 stat2) = DSequence (desugar stat) (DWhile ex (DSequence (desugar stat2) (desugar stat1)))
desugar (Sequence stat stat1) = DSequence (desugar stat) (desugar stat1)
desugar Skip = DSkip

evalSimple :: State -> DStatement -> State
evalSimple st (DAssign str ex) = extend st str (eval st ex)
evalSimple st (DIfThen ex ds) = if eval st ex == 1
  then evalSimple st ds
evalSimple st (DIfThenElse ex ds ds1) = if eval st ex == 1
  then evalSimple st ds
  else evalSimple st ds1
evalSimple st (DWhile ex ds) = if eval st ex == 1
  then evalSimple (evalSimple st ds) (DWhile ex ds)
  else evalSimple st DSkip
evalSimple st (DSequence ds ds1) = evalSimple (evalSimple st ds) ds1
evalSimple st DSkip = st

run :: State -> Statement -> State
run st = evalSimple st . desugar










-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Sub (Val 1)))
                (Assign "Out" (Op (Var "In") Mul (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Mul (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Sub (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Add (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
