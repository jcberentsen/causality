{-# LANGUAGE OverloadedStrings, TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}

import Test.Tasty
--import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.TH

import Model

fact = testCase "A fact is true" $ False @?= (isFact $ Fact "universal truth")

--test_implication = Implication (Fact "I think") (Fact "I am") -- cogito ergo sum
--premise_implies_conclusion = testCase "premise implies conclusion" $ True @?= isFact $ eval test_implication

main :: IO ()
main = defaultMain $(testGroupGenerator)
