module Ast where

import Data.List (intercalate)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map



data Binop = And | Or | Impl deriving(Eq, Ord)

instance Show Binop where
    show = \case
        And -> " /\\ "
        Or -> " \\/ "
        Impl -> " \\-> "

data Formula = Binop Binop Formula Formula
             | Not Formula
             | Var String
             deriving(Eq, Ord)

instance Show Formula where
    showsPrec prec = \case
        Binop b l r -> case b of
            And -> showParen (prec > 3) $ showsPrec 3 l . (show b ++) . showsPrec 4 r
            Or -> showParen (prec > 2) $ showsPrec 2 l . (show b ++) . showsPrec 3 r
            Impl -> showParen (prec > 1) $ showsPrec 2 l . (show b ++) . showsPrec 1 r
        Not e -> showParen (prec > 4) $ ("neg "++) . showsPrec 4 e
        Var x -> showString x

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

data Sequent = Sequent { sassumptions :: [Formula], sconclusions :: [Formula] } deriving(Eq, Ord)

instance Show Sequent where
    show Sequent{..} = intercalate ", " (show <$> sassumptions) ++ " |- " ++ intercalate ", " (show <$> sconclusions)
    showList ss = showString (intercalate "\n" (show <$> ss))

logicOfBinop :: Binop -> Bool -> Bool -> Bool
logicOfBinop = \case
    And -> (&&)
    Or -> (||)
    Impl -> (\a b -> not a || b)

eval :: [(String, Bool)] -> Formula -> Either String Bool
eval env = let go = eval env in \case
    Var x -> case lookup x env of
        Nothing -> Left x
        Just b -> return b
    Not e -> not <$> go e
    Binop b l r -> logicOfBinop b <$> go l <*> go r

evalm :: Map String Bool -> Formula -> Either String Bool
evalm env = let go = evalm env in \case
    Var x -> case Map.lookup x env of
        Nothing -> Left x
        Just b -> return b
    Not e -> not <$> go e
    Binop b l r -> logicOfBinop b <$> go l <*> go r


freeVars :: Formula -> Set String
freeVars = \case
    Var x -> Set.singleton x
    Binop _ l r -> freeVars l <> freeVars r
    Not e -> freeVars e