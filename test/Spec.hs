import Test.Hspec

import Ast
import Reduction


(@=) :: (HasCallStack, Eq a, Show a) => a -> a -> Expectation
(@=) = shouldBe

tProve :: HasCallStack => Formula -> Status -> Expectation
tProve a b = tryProve a @= b

main :: IO ()
main = hspec $ do
    describe "formula prover" $ do
        it "can't prove p" $ do
            tryProve p @= NonProvable
            tryProve q @= NonProvable
        it "can prove application" $ do
            tryProve (p \-> (p \-> q) \-> q) @= Provable
            tryProve ((p \-> q) \-> p \-> q) @= Provable
