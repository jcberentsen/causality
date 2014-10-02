{-# LANGUAGE OverloadedStrings, TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}

import Test.Tasty
--import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.TH

import Model

myTestGroup = $(testGroupGenerator)

case_fact = do True @?= (isFact $ Fact "universal truth")

an_implication = Implication (Fact "I think") (Fact "I am") -- cogito ergo sum
case_premise_implies_conclusion = do True @?= (isFact $ eval an_implication)

main :: IO ()
main = defaultMain myTestGroup

