{-# LANGUAGE GADTs #-}
module AST where
data Type = INT | CHAR | FLOAT | DOUBLE | POINTER Type | Rec String [Type] deriving (Eq,Show)
data Decl = V Vardec
          | R Recdec
data Prog = Prog [Decl] [Func]
data Vardec = Vardec String Type
data Recdec = Recdec String [Vardec]
data Func = Func Type String [Vardec] [Stmt]
data Lit a where
         LInt    :: Int    -> Lit Int
         LFloat  :: Float  -> Lit Float
         LDouble :: Double -> Lit Double
         LChar   :: Char   -> Lit Char
         LString :: String -> Lit String
         LBool   :: Bool   -> Lit Bool
         deriving (Eq, Show)

data Expr = L Lit
          | Var String
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Divided Expr Expr
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
          | Equal Expr Expr
          deriving (Eq, Show)

data Stmt = Assign String Expr
          | If Expr [Stmt] (Maybe [Stmt])
          | While Expr [Stmt]
          | For Stmt Expr Stmt [Stmt]
          | Declare String Expr
