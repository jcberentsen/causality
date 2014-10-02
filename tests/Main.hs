{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}

import Test.Tasty
--import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.TH

isFact :: Bool
isFact = True

myTestGroup :: TestTree
myTestGroup = $(testGroupGenerator)

premise_implies_conclusion = testCase "premise implies conclusion" $ True @?= isFact

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "sanity" $ True @?= True
    , myTestGroup
    ]
