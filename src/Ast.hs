module Ast where

data Binop = And | Or | Impl deriving(Eq, Ord, Show)

data Formula = Binop Binop Formula Formula
             | Not Formula
             | Var String
             deriving(Eq, Ord, Show)

p :: Formula
p = Var "p"

q :: Formula
q = Var "q"

infixl 3 /\
infixl 2 \/
infixr 1 \->
infixl 0 <->

(/\) :: Formula -> Formula -> Formula
(/\) = Binop And

(\/) :: Formula -> Formula -> Formula
(\/) = Binop Or

(\->) :: Formula -> Formula -> Formula
(\->)=Binop Impl

(<->) :: Formula -> Formula -> Formula
a <-> b = (a \-> b) /\ (b \-> a)

neg :: Formula -> Formula
neg = Not

data Sequent = Sequent { sassumptions :: [Formula], sconclusions :: [Formula] } deriving(Eq, Ord, Show)

