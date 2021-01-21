module Reduction where

import Ast

data Status = Reducible | NonProvable | Provable deriving(Eq, Ord, Show)

combineStatus :: Status -> Status -> Status
combineStatus = max

getStatus :: Sequent -> Status
getStatus Sequent{..} =
    if
        | allAtomic && f sassumptions sconclusions -> Provable
        | allAtomic -> NonProvable
        | otherwise -> Reducible
    where
        isAtomic = \case
            Var{} -> True
            Binop{} -> False
            Not{} -> False
        allAtomic = all isAtomic sconclusions && all isAtomic sassumptions
        -- | is at least one thing on the right in the left?
        f _ [] = False
        f ls rs = any (`elem` ls) rs

-- | insert x xs inserts x into xs, and keeps xs sorted (assumes xs is sorted)
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (x':xs)
    | x <= x' = x:x':xs
    | otherwise = x':insert x xs

-- | insert xs ys inserts each x into ys (assumes ys is sorted)
inserts :: (Foldable t, Ord a) => t a -> [a] -> [a]
inserts xs ys = foldr insert ys xs

stepconc :: Sequent -> [Sequent]
stepconc s@Sequent{..} = case sconclusions of
    [] -> [s]
    Var{}:_ -> [s] -- assumes sorted, => all concs are irreducible
    Not e:concs -> [Sequent (insert e sassumptions) concs]
    Binop And l r:concs ->
        [Sequent sassumptions (insert l concs), Sequent sassumptions (insert r concs)]
    Binop Or l r :concs ->
        [Sequent sassumptions (inserts [l,r] concs)]
    Binop Impl l r:concs ->
        [Sequent (insert l sassumptions) (insert r concs)]

stepass :: Sequent -> [Sequent]
stepass s@Sequent{..} = case sassumptions of
    [] -> [s]
    Var{}:_ -> [s] -- assumes sorted, => all concs are irreducible
    Not e:asses -> [Sequent asses (insert e sconclusions)]
    Binop And l r:asses ->
        [Sequent (inserts [l,r] asses) sconclusions]
    Binop Or l r:asses ->
        [Sequent (insert l asses) sconclusions, Sequent (insert r asses) sconclusions]
    Binop Impl l r:asses ->
        [Sequent asses (insert l sconclusions), Sequent (insert r asses) sconclusions]

repeatUntilIdempotent :: Eq t => (t -> t) -> t -> t
repeatUntilIdempotent f x =
    let go = repeatUntilIdempotent f
        x' = f x
    in if x == x' then x' else go x'

reduce :: [Sequent] -> [Sequent]
reduce =
    let reduceass = repeatUntilIdempotent (concatMap stepass)
        reduceconc = repeatUntilIdempotent (concatMap stepconc)
    in reduceass . reduceconc

-- | Is this formula true for all truth values of variables?
tryProve :: Formula -> Status
tryProve = minimum . fmap getStatus . reduce . return . (\f -> Sequent [] [f])