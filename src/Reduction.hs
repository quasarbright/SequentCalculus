{-# LANGUAGE FlexibleInstances #-}

module Reduction where

import Ast
import Control.Arrow ((>>>))
import Data.Set(Set)
import qualified Data.Set as Set
import Latex

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

data Rose a = Rose a [Rose a] deriving(Eq, Ord, Show)

instance Functor Rose where
    fmap f (Rose a children) = Rose (f a) (fmap (fmap f) children)

instance Applicative Rose where
    pure a = Rose a []
    Rose f rfs <*> Rose x rxs = Rose (f x) [rf <*> rx | rf <- rfs, rx <- rxs]

instance Latex (Rose Sequent) where
    latex (Rose a rs) = "\\frac{\\displaystyle "++latex a++"}{\\displaystyle "++latexList rs "}"
    latexList [] = showString ""
    latexList [r] = showString (latex r)
    latexList (r:rs) = (latex r++) . ("\\qquad "++) . latexList rs

treeConc :: Sequent -> Rose Sequent
treeConc s = let ss = stepconc s in
    case ss of
        [s'] | s == s' -> treeAss s
        _ -> Rose s (fmap treeConc ss)

treeAss :: Sequent -> Rose Sequent
treeAss s = let ss = stepass s in
    case ss of
        [s'] | s == s' -> pure s
        _ -> Rose s (fmap treeAss ss)

reduceTree :: Sequent -> Rose Sequent
reduceTree = treeConc

initSequent :: Formula -> Sequent
initSequent f = Sequent [] [f]

-- | Is this formula true for all truth values of variables?
tryProve :: Formula -> Status
tryProve = minimum . fmap getStatus . reduce . return . initSequent

-- | Is this formula satisfiable?
sat :: Formula -> Bool
sat f = case tryProve (Not f) of
    Provable -> False -- f is never true
    NonProvable -> True -- f may be true
    Reducible -> error "impossible"

-- | reduce this formula to atomic sequents
finalSequents :: Formula -> [Sequent]
finalSequents = initSequent >>> return >>> reduce

sat_ :: Formula -> [Sequent]
sat_ = neg >>> finalSequents >>> filter (\s -> getStatus s == NonProvable)

satcnf_ :: [[Formula]] -> [Sequent]
satcnf_ = cnf >>> sat_

getFVar :: Formula -> Set String
getFVar (Var x) = Set.singleton x
getFVar _ = mempty

getFsVars :: [Formula] -> Set String
getFsVars = mconcat . fmap getFVar

-- | get all (atomic) vars occurring on left of turnstile, and all vars on right of turnstile.
getSVars :: Sequent -> (Set String, Set String)
getSVars Sequent{..} = (mconcat $ fmap getFVar sassumptions, mconcat $ fmap getFVar sconclusions)

-- | get the assignments of a sequent, assuming trues are on the left and falses are on the right.
getSSols :: Sequent -> [(String, Bool)]
getSSols s = case getSVars s of
    (trues,falses) -> Set.toList $ Set.map (,True) trues <> Set.map (, False) falses

--sat_solve f =
--    let seqs = satcnf_ f
--        seps = 

cnf :: [[Formula]] -> Formula
cnf = foldl1 (/\) . fmap (foldl1 (\/))

checkSat :: (Formula -> [(String, Bool)]) -> Formula -> Either String Bool
checkSat satfun f = eval (satfun f) f
    


{-
now you have an O(n^2) sat decider, I think. But how do you extract the solution?
conjecture: reduce the sequent. Look at the non-provable sequents. maybe that tells you something?
I've done some manual testing, and noticed a pattern: variables which have to be unequal end up on opposite
sides of the turnstile. And variables which have to be equal end up on the same side.

It's actually not O(n^2) bc of splits.
It's O(m^n) for n clauses of size m.

damn, I think It's left true right false.

was messing around and got this:
╬╗> sat_ (neg q <-> neg p)
 |- p, q
(excluded p,q |-)

but
╬╗> sat_ (q /\ p \/ neg p /\ neg q)
p, q |-
 |- p, q
so that's good. But I'm suspicious. At least the left-true-right-false interpretation produces A solution.
╬╗> tryProve ((p <-> q) <-> (p/\q \/ neg p/\neg q))
Provable
It seems equivalent statements can produce different final sequents. Better be careful with assumptions
when testing. Should only test cnf expressions.

In this, p and q occur on both sides. You need to be careful when you generate assignments from sequents.
Might be its own sat :(. I bet it just reduces the sat to a simpler one.
Pretty sure if you say left true right false for each sequent, it's the or of those assignments.

it's possible that the rhs is an or (or the lhs bc negation?) and i've only seen simple cases where
that doesn't manifest. Keep an eye out and do bigger tests. Learn quick check so you don't have to think lol.
Idk what I'm talking abt rn, late at night.

TODO think about reduction conserves and see if the rhs of turnstile is still an or in the reduced sequents.

╬╗> satcnf_ [[p,q],[neg q,neg p],[p,r]]
p, p |- q
p, r |- q
q, r |- p
☻*Main Ast Lib Paths_SequentCalculus Reduction
╬╗> sat . cnf $ [[p,q],[neg q,neg p],[p,r]]
True
this should be (p /= q) /\ (p \/ r)
so (p=t,q=f,r=t/f) or (p=f,q=t,r=t) should be good. I verified these as solutions with eval.
Interesting how r is always true in the sequents. Even with cnf, not all assignments will be found.
Not looking good. Might not even get a reduction to an easier sat at this point.

But these sequents seem to say (p=t,q=f) or (p=t,r=t,q=f) or (q=t,r=t,p=t) which is dnf. Interesting.

Just realized reduction is exponential for sat. But it would be cool to make a solver anyway.

reduction just produces a dnf expression, which you also have to solve. Screw this.

-}

-- TODO if you want to render the inference tree, nested inferences can be rendered as nested \frac{}{} latex commands
-- with \qquad between them! Not actually that bad