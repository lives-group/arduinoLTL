{-# LANGUAGE GADTs #-}
module AST where
data Type = INT | CHAR | FLOAT | DOUBLE | POINTER Type | Rec String [Type] deriving (Eq,Show)
data Decl = V Vardec
          | R Recdec
data Prog = Prog [Decl] [Func]
data Vardec = Vardec String Type
data Recdec = Recdec String [Vardec]
data Func = Func Type String [Vardec] [Stmt]

data Expr a where
            LInt    :: Int    -> Expr Int
            LFloat  :: Float  -> Expr Float
            LDouble :: Double -> Expr Double
            LChar   :: Char   -> Expr Char
            LString :: String -> Expr String
            LBool   :: Bool   -> Expr Bool
            
            --Var     :: String 
            
            Plus    :: Expr Int -> Expr Int -> Expr Int
            Minus   :: Expr Int -> Expr Int -> Expr Int
            Times   :: Expr Int -> Expr Int -> Expr Int
            Divided :: Expr Int -> Expr Int -> Expr Int
            
            Plus    :: Expr Float -> Expr Float -> Expr Float
            Minus   :: Expr Float -> Expr Float -> Expr Float
            Times   :: Expr Float -> Expr Float -> Expr Float
            Divided :: Expr Float -> Expr Float -> Expr Float
            
            Plus    :: Expr Double -> Expr Double -> Expr Double
            Minus   :: Expr Double -> Expr Double -> Expr Double
            Times   :: Expr Double -> Expr Double -> Expr Double
            Divided :: Expr Double -> Expr Double -> Expr Double
            
            Not     :: Expr Bool -> Expr Bool
            And     :: Expr Bool -> Expr Bool -> Expr Bool
            Or      :: Expr Bool -> Expr Bool -> Expr Bool
            Equal   :: Expr a ->  Expr a -> Expr a
          deriving (Eq, Show)

data Stmt = Assign String Expr
          | If Expr [Stmt] (Maybe [Stmt])
          | While Expr [Stmt]
          | For Stmt Expr Stmt [Stmt]
          | Declare String Expr
