module LTL where
import Control.Monad
import Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as Map

data LTL = Atom String
         | TT | FF
         | LTL :&: LTL
         | LTL :|: LTL
         | Not LTL
         | G LTL
         | F LTL
         | X LTL
         | U LTL LTL
         | W LTL LTL
         | R LTL LTL
         deriving (Eq, Ord, Show)

type Env a = Map String a

type Interp a = String -> a -> Bool

lookupVal :: Interp a -> String -> [Env a] -> Bool
lookupVal interp s (x : xs) = case Map.lookup s x of
                                 Just y  -> interp s y
                                 Nothing -> False

checkLTL :: Interp a -> LTL -> [Env a] -> Bool
checkLTL interp TT ss = True
checkLTL interp FF ss = False
checkLTL interp (Atom s) ss = lookupVal interp s ss
checkLTL interp (l :&: r) ss = (checkLTL interp l ss) && (checkLTL interp r ss)
checkLTL interp (l :|: r) ss = (checkLTL interp l ss) || (checkLTL interp r ss)
checkLTL interp (Not l) ss = not (checkLTL interp l ss)
checkLTL interp (G l) ss = all (checkLTL interp l . (:[])) ss
checkLTL interp (F l ) ss = or (map (checkLTL interp l . (:[])) ss)
checkLTL interp (X l) ss = checkLTL interp l (tail ss)
checkLTL interp (U l r) ss = let
                               ss' = dropWhile (checkLTL interp l . (: [])) ss
                             in checkLTL interp r ss'
checkLTL interp (W l r) ss = checkLTL interp ((U l r) :|: (G l)) ss
checkLTL interp (R l r) ss = checkLTL interp (W r (l :&: r)) ss

type Var = String

data Stmt = Assign Var Exp

type Prog = [Stmt]

data Exp = EVar Var | Lit Int | Exp :+: Exp deriving (Eq, Ord, Show)

exec :: Prog -> [Env Int]
exec p = execState (exec' p) [Map.empty]

exec' :: Prog -> State ([Env Int]) ()
exec' = mapM_ execStmt

execStmt :: Stmt -> State ([Env Int]) ()
execStmt (Assign v e) = do
                          i <- eval e
                          modify (\(x:xs) -> (Map.insert v i x) : x : xs)

eval :: Exp -> State ([Env Int]) Int
eval (EVar v) = gets ((Map.! v) . head)
eval (Lit i) = return i
eval (e :+: e') = liftM2 (+) (eval e) (eval e')


example :: Prog
example = [Assign "x" (Lit 0), Assign "y" ((EVar "x") :+: (Lit 1))
          , Assign "x" ((EVar "y") :+: (Lit 1))]

cond :: Interp Int
cond s i = s == "x" && i < 0

prop :: LTL
prop = F (Atom "x")
