{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

-- cabal test --ghc-option=-v0
-- cabal configure --enable-tests && cabal build v0 && dist/build/causality-tests/causality-tests

import Test.Tasty
--import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.TH
import Data.DeriveTH

import Model

--import Data.Derive.Arbitrary
--import Test.QuickCheck.Arbitrary

derive makeArbitrary ''Probability
derive makeArbitrary ''Sure
derive makeArbitrary ''Evidence
derive makeArbitrary ''Fact
derive makeArbitrary ''Implication

myTestGroup = $(testGroupGenerator)

cogito_ergo_sum = Implication (fact "I think") (fact "I am")
case_cogito_ergo_sum = do True @?= (isFact $ eval cogito_ergo_sum)

prop_fact f = isFact f
    where _type = f :: Fact Bool

prop_implication imp = isFact $ eval imp
    where _types = imp :: Implication Bool

main :: IO ()
main = defaultMain myTestGroup

