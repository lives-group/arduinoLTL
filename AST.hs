{-# LANGUAGE GADTs #-}
module AST where
data Type = INT | CHAR | FLOAT | DOUBLE | POINTER Type | Rec String [Type] deriving (Eq,Show)
data Decl = V Vardec
          | R Recdec
data Prog = Prog [Decl] [Func]
data Vardec = Vardec String Type
data Recdec = Recdec String [Vardec]
data Func = Func Type String [Vardec] [Stmt]

data ValueNum a where
            LInt :: Int -> ValueNum Int
            LFloat :: Float -> ValueNum Float
            LDouble :: Double -> ValueNum Double

data ValueChar a where =
            LChar :: Char -> ValueChar Char

data ValueString a where= LString String
            String :: String -> ValueString String

data ValueBool a where
            LBool :: Bool -> ValueBool Bool

data Expr a where
            Plus    :: ValueNum a -> ValueNum a-> Expr a
            Minus   :: ValueNum a -> ValueNum a-> Expr a
            Times   :: ValueNum a-> ValueNum a-> Expr a
            Divided :: ValueNum a-> ValueNum a-> Expr a

            Not     :: ValueBool b -> Expr b
            And     :: ValueBool b-> ValueBool b -> Expr b
            Or      :: ValueBool b-> ValueBool b -> Expr b
            --Definir outros contrutores para Equal
            Equal   :: ValueBool b-> ValueBool b -> Expr b
          deriving (Show)

data Stmt = Assign String Expr
          | If Expr [Stmt] (Maybe [Stmt])
          | While Expr [Stmt]
          | For Stmt Expr Stmt [Stmt]
          | Declare String Expr
