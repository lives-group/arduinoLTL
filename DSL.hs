-- DSL
Assign :: String -> Expr -> Vardec
If :: Expr -> [Stmt] -> Maybe [Stmt]
While :: Expr -> [Stmt]
For :: Stmt -> Expr -> Stmt -> [Stmt]
Declare :: String -> Expr -> Vardec
