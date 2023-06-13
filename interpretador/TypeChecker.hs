module TypeChecker where 

import Data.List
import Data.Maybe
import Lexer 

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty 
typeof ctx BTrue = Just TBool
typeof ctx BFalse = Just TBool
typeof ctx (Num _) = Just TNum
typeof ctx (Plus e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                            (Just TNum, Just TNum) -> Just TNum 
                            _                      -> Nothing 
typeof ctx (Minus e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                             (Just TNum, Just TNum) -> Just TNum 
                             _                      -> Nothing 
typeof ctx (Times e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                             (Just TNum, Just TNum) -> Just TNum 
                             _                      -> Nothing 
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TBool, Just TBool) -> Just TBool 
                           _                      -> Nothing 
typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                          (Just TBool, Just TBool) -> Just TBool 
                          _                      -> Nothing 
typeof ctx (Paren e) = typeof ctx e 
typeof ctx (If e1 e2 e3) = case typeof ctx e1 of 
                             Just TBool -> case (typeof ctx e2, typeof ctx e3) of 
                                             (Just t2, Just t3) -> if (t2 == t3) then
                                                                     Just t2
                                                                   else 
                                                                     Nothing
                             _          -> Nothing
typeof ctx (Var v) = lookup v ctx 
typeof ctx (Lam v t1 b) = let Just t2 = typeof ((v, t1):ctx) b 
                            in Just (TFun t1 t2)
typeof ctx (App e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                            (Just (TFun t11 t12), Just t2) -> if (t11 == t2) then
                                                                Just t12 
                                                              else 
                                                                Nothing
                            _                              -> Nothing
typeof ctx (Let v e1 e2) = case typeof ctx e1 of
                             Just t1 -> typeof ((v, t1):ctx) e2 
                             _       -> Nothing
typeof ctx (Tuple el) = let tl = map (typeof ctx) el 
                          in if (length tl == length (catMaybes tl)) then
                               Just (TTuple (catMaybes tl))
                             else 
                               Nothing
typeof ctx (TupleProj e1 n) = case typeof ctx e1 of 
                                Just (TTuple tl) -> if (n > 0 && n <= length tl) then
                                               Just (tl !! (n - 1))
                                             else 
                                               Nothing
                                _         -> Nothing 
typeof ctx (Record rl) = let (ll, el) = unzip rl 
                             tl = map (typeof ctx) el
                          in if (length tl == length (catMaybes tl)) then
                               Just (TRecord (zip ll (catMaybes tl)))
                             else 
                               Nothing
typeof ctx (RecordProj e1 l) = case typeof ctx e1 of 
                                Just (TRecord tl) -> lookup l tl
                                _                 -> Nothing
typeof ctx (Fix e) = case typeof ctx e of 
                       Just (TFun t1 t2) -> if (t1 == t2) then
                                              Just t1
                                            else 
                                              Nothing
                       _                 -> Nothing
typeof ctx (Eq e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                          (Just t1, Just t2) -> if t1 == t2 then
                                                  Just TBool
                                                else 
                                                  Nothing
                          _                  -> Nothing
typeof ctx (Not e) = case (typeof ctx e) of 
                       (Just TBool) -> Just TBool 
                       _            -> Nothing
typeof ctx (Nil t) = Just (List t)
typeof ctx (Cons t e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                              (Just t1, Just (List t2)) -> if t1 == t2 then 
                                                             Just (List t1)
                                                           else 
                                                             Nothing
                              _                         -> Nothing
typeof ctx (IsNil t e) = case typeof ctx e of 
                           Just (List t') -> if t == t' then 
                                               Just TBool
                                             else 
                                               Nothing 
                           _               -> Nothing 
typeof ctx (Head t e) = case typeof ctx e of 
                          Just (List t') -> if t == t' then 
                                               Just t
                                             else 
                                               Nothing 
                          _               -> Nothing 
typeof ctx (Tail t e) = case typeof ctx e of 
                          Just (List t') -> if t == t' then 
                                               Just (List t)
                                             else 
                                               Nothing 
                          _               -> Nothing 


typecheck :: Expr -> Expr 
typecheck e = case (typeof [] e) of
                Just _ -> e 
                _      -> error "Type error: erro na verificação de tipos!"
