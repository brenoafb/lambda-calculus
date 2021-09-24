module Eval where

import Syntax

eval :: Term -> Maybe Term
eval (App t1 t2) = do
  e1 <- eval t1
  e2 <- eval t2
  apply e1 e2
eval x = pure x

apply :: Term -> Term -> Maybe Term
apply (Atom y) x  = Nothing
apply (Abs v b) x = eval $ subs v x b
apply (App t1 t2) x = do
  e <- eval (App t1 t2)
  apply e x

subs :: Ident -> Term -> Term -> Term
subs v t (Atom y)
  | v == y    = t
  | otherwise = Atom y
subs v t (Abs p b) = Abs p $ subs v t b
subs v t (App t1 t2) = App (subs v t t1) (subs v t t2)
