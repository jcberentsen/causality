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

derive makeArbitrary ''Probability
derive makeArbitrary ''Evidence
derive makeArbitrary ''Causality

myTestGroup = $(testGroupGenerator)

evidence_for_falling :: Evidence String Int
evidence_for_falling = Evidence "falling" yes

gravity_is_a_fact :: Evidence String Int
gravity_is_a_fact = Evidence "gravity" yes

gravity_causes_falling = Causality "gravity" "falling"
case_gravity = do evidence_for_falling @?= eval gravity_causes_falling gravity_is_a_fact
    where _types = yes :: Probability Integer

prop_evidence_for_cause_causally_yields_effect (causality@(Causality cause effect)) = effect_caused  == (eval causality (Evidence cause yes))
    where _types = (causality :: Causality Bool, yes :: Probability Integer)
          effect_caused = (Evidence effect yes) :: Evidence Bool Integer

main :: IO ()
main = defaultMain myTestGroup

