module Interpreter where 

import Lexer 

rename :: String -> String -> Expr -> Expr -> Expr
rename xo x n e@(Var v) = if v == x then
                            Var (xo ++ x)
                          else 
                            e
rename xo x n (Lam v t b) = if v == x then
                              Lam (xo ++ x) t (rename xo x n b)
                            else 
                              Lam v t (rename xo x n b)
rename xo x n (App e1 e2) = App (rename xo x n e1) (rename xo x n e2)
rename xo x n (Plus e1 e2) = Plus (rename xo x n e1) (rename xo x n e2)
rename xo x n (Minus e1 e2) = Minus (rename xo x n e1) (rename xo x n e2)
rename xo x n (Times e1 e2) = Times (rename xo x n e1) (rename xo x n e2)
rename xo x n (And e1 e2) = And (rename xo x n e1) (rename xo x n e2)
rename xo x n (Or e1 e2) = Or (rename xo x n e1) (rename xo x n e2)
rename xo x n (Paren e) = Paren (rename xo x n e)
rename xo x n (If e1 e2 e3) = If (rename xo x n e1) (rename xo x n e2) (rename xo x n e3)
rename xo x n (Let v e1 e2) = Let v (rename xo x n e1) (rename xo x n e2)
rename xo x n (Tuple el) = Tuple (map (rename xo x n) el)
rename xo x n (TupleProj e i) = TupleProj (rename xo x n e) i
rename xo x n (Record rl) = Record (map (\(l,e) -> (l, rename xo x n e)) rl)
rename xo x n (RecordProj e i) = RecordProj (rename xo x n e) i
rename xo x n (Fix e) = Fix (rename xo x n e)
rename xo x n (Eq e1 e2) = Eq (rename xo x n e1) (rename xo x n e2)
rename xo x n (Not e) = Not (rename xo x n e)
rename xo x n (Cons t e1 e2) = Cons t (rename xo x n e1) (rename xo x n e2)
rename xo x n (IsNil t e) = IsNil t (rename xo x n e)
rename xo x n (Head t e) = Head t (rename xo x n e)
rename xo x n (Tail t e) = Tail t (rename xo x n e)
rename xo x n e = e

subst :: String -> Expr -> Expr -> Expr
subst x n b@(Var v) = if v == x then
                        n
                      else 
                        b
subst x n (Lam v t b) = Lam v t (subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
subst x n (Plus e1 e2) = Plus (subst x n e1) (subst x n e2)
subst x n (Minus e1 e2) = Minus (subst x n e1) (subst x n e2)
subst x n (Times e1 e2) = Times (subst x n e1) (subst x n e2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (Or e1 e2) = Or (subst x n e1) (subst x n e2)
subst x n (Paren e) = Paren (subst x n e)
subst x n (If e1 e2 e3) = If (subst x n e1) (subst x n e2) (subst x n e3)
subst x n (Let v e1 e2) = Let v (subst x n e1) (subst x n e2)
subst x n (Tuple el) = Tuple (map (subst x n) el)
subst x n (TupleProj e i) = TupleProj (subst x n e) i
subst x n (Record rl) = Record (map (\(l,e) -> (l, subst x n e)) rl)
subst x n (RecordProj e i) = RecordProj (subst x n e) i
subst x n (Fix e) = Fix (subst x n (rename x x n e))
subst x n (Eq e1 e2) = Eq (subst x n e1) (subst x n e2)
subst x n (Not e) = Not (subst x n e)
subst x n (Cons t e1 e2) = Cons t (subst x n e1) (subst x n e2)
subst x n (IsNil t e) = IsNil t (subst x n e)
subst x n (Head t e) = Head t (subst x n e)
subst x n (Tail t e) = Tail t (subst x n e)
subst x n e = e

is_value :: Expr -> Bool
is_value BTrue = True
is_value BFalse = True
is_value (Num _) = True 
is_value (Lam _ _ _) = True
is_value (Tuple el) = all (is_value) el
is_value (Record rl) = all (\(l,e) -> is_value e) rl
is_value (Nil _) = True 
is_value (Cons _ e1 e2) = is_value e1 && is_value e2 
is_value _ = False

step :: Expr -> Expr
step (Plus (Num n1) (Num n2)) = Num (n1 + n2)
step (Plus (Num n1) e2) = Plus (Num n1) (step e2)
step (Plus e1 e2) = Plus (step e1) e2
step (Minus (Num n1) (Num n2)) = Num (n1 - n2)
step (Minus (Num n1) e2) = Minus (Num n1) (step e2)
step (Minus e1 e2) = Minus (step e1) e2
step (Times (Num n1) (Num n2)) = Num (n1 * n2)
step (Times (Num n1) e2) = Times (Num n1) (step e2)
step (Times e1 e2) = Times (step e1) e2
step (And BTrue e2) = e2 
step (And BFalse e2) = BFalse 
step (And e1 e2) = And (step e1) e2
step (Or BTrue e2) = BTrue 
step (Or BFalse e2) = e2 
step (Or e1 e2) = Or (step e1) e2
step (Paren e) = e
step (If BTrue e2 e3) = e2
step (If BFalse e2 e3) = e3
step (If e1 e2 e3) = If (step e1) e2 e3
step (App e1@(Lam x _ b) e2) | is_value e2 = subst x e2 b 
                             | otherwise     = (App e1 (step e2))
step (App e1 e2) = App (step e1) e2 
step (Let x e1 e2) | is_value e1 = subst x e1 e2 
                   | otherwise   = (Let x (step e1) e2)
step (TupleProj e@(Tuple vl) n) | is_value e = vl !! (n - 1)
                                | otherwise  = TupleProj (step e) n
step (Tuple el) = Tuple (map step el)
step (RecordProj e@(Record vl) l) | is_value e = case (lookup l vl) of 
                                                   Just e -> e 
                                  | otherwise = RecordProj (step e) l 
step (Record rl) = Record (map (\(l,e) -> (l, step e)) rl)
step (Fix e@(Lam x t1 e2)) = subst x (Fix e) e2
step (Fix e) = Fix (step e)
step (Eq e1 e2) | is_value e1 && is_value e2 = if (e1 == e2) then
                                                 BTrue 
                                               else 
                                                 BFalse 
                | is_value e1 = Eq e1 (step e2)
                | otherwise = Eq (step e1) e2
step (Not BTrue) = BFalse
step (Not BFalse) = BTrue 
step (Not e) = Not (step e)
step (Cons t e1 e2) | is_value e1 = Cons t e1 (step e2)
                    | otherwise = Cons t (step e1) e2 
step (IsNil _ (Nil _)) = BTrue 
step (IsNil _ (Cons _ _ _)) = BFalse 
step (IsNil t e) = IsNil t (step e)
step (Head _ (Nil _)) = error "Calling 'head' with a nil list!"
step (Head _ (Cons _ e1 e2)) = e1 
step (Head t e) = Head t (step e)
step (Tail _ (Nil _)) = error "Calling 'tail' with a nil list!"
step (Tail _ (Cons _ e1 e2)) = e2 
step (Tail t e) = Tail t (step e)
step e = e 

eval :: Expr -> Expr 
eval e | is_value e = e
       | otherwise  = eval (step e)
