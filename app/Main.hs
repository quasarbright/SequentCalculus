module Main where

import Reduction
import Latex

main :: IO ()
main = putStrLn . latex . reduceTree . initSequent $ cnf [[p,q,q],[p, neg q, neg q], [neg q, p, p]]
