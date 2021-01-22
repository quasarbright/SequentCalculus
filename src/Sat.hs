module Sat where

import Ast
import Data.Map(Map)
import qualified Data.Map as Map
--import Data.Set(Set)
import qualified Data.Set as Set


assignments :: Ord k => [k] -> [Map k Bool]
assignments [] = [mempty]
assignments (x:vars) = let rest = assignments vars in
    fmap (Map.insert x False) rest <> fmap (Map.insert x True) rest

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

satisfy :: Formula -> Maybe (Map String Bool)
satisfy f = safeHead [ass | ass <- assignments vars, evalm ass f == Right True]
    where vars = Set.toList (freeVars f)