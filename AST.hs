{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-} -- Usando isso para corrigir erro em Show em Expr
module AST where
data Type = INT | CHAR | FLOAT | DOUBLE | POINTER Type | Rec String [Type] deriving (Eq,Show)
data Decl = V Vardec   --One vou usar isso? Variaveis globais?
          | R Recdec
data (Prog a) = Prog [Decl] [(Func a)] -- Essa variavel fantasma vai ser oque neste ponto?
data Vardec = Vardec String Type
data Recdec = Recdec String [Vardec]
data Func a = Func Type String [Vardec] [(Stmt a)]


data Expr a where

            LInt    :: Int -> Expr Int
            LFloat  :: Float -> Expr Float
            LDouble :: Double -> Expr Double
            LChar   :: Char -> Expr Char
            LBool   :: Bool -> Expr Bool

            Plus    :: (Num a, Show a) => Expr a -> Expr a -> Expr a
            Minus   :: (Num a, Show a) => Expr a -> Expr a -> Expr a
            Times   :: (Num a, Show a) => Expr a -> Expr a -> Expr a
            Divided :: (Num a, Show a) => Expr a -> Expr a -> Expr a

            Not     :: Expr Bool -> Expr Bool
            And     :: Expr Bool -> Expr Bool -> Expr Bool
            Or      :: Expr Bool -> Expr Bool -> Expr Bool

            --Equal vai ser para qualquer tipo?
            Equal   ::Show a => Expr a-> Expr a -> Expr Bool
          deriving (Show) -- Apenas Show?

data Stmt a = Assign String (Expr a)  -- Variavel Fantasma aqui?
          | If (Expr Bool) [(Stmt a)] (Maybe [(Stmt a)])
          | While (Expr Bool) [(Stmt a)]
          | For (Stmt a) (Expr a) (Stmt a) [(Stmt a)] --Qual os tipos aqui?
          | Declare String (Expr a) -- Isso é Realmente necessario?
