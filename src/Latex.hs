module Latex where

class Latex a where
    latex :: a -> String
    latexsPrec :: Int -> a -> ShowS
    latexList :: [a] -> ShowS
    latex a = latexsPrec 0 a ""
    latexsPrec _ = showString . latex
    latexList [] = showString ""
    latexList [x] = showString (latex x)
    latexList (x:xs) = (latex x ++) . (", " ++) . latexList xs
    {-# MINIMAL latex | latexsPrec #-}

instance Latex a => Latex [a] where
    latex xs = latexList xs ""