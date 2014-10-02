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

gravity = Causality "gravity" "falling"

case_gravity_causes_falling = do evidence_for_falling @?= eval gravity gravity_is_a_fact

gravity_is_a_fact :: Evidence String Int
gravity_is_a_fact = Evidence "gravity" yes

evidence_for_falling :: Evidence String Int
evidence_for_falling = Evidence "falling" yes

case_no_gravity_causes_no_falling = do eval gravity universe_with_no_gravity @?= no_evidence_for_falling

universe_with_no_gravity :: Evidence String Int
universe_with_no_gravity = Evidence "gravity" no

no_evidence_for_falling :: Evidence String Int
no_evidence_for_falling = Evidence "falling" no

prop_evidence_for_cause_causally_yields_effect (causality@(Causality cause effect)) = effect_caused  == (eval causality (Evidence cause yes))
    where _types = (causality :: Causality Bool, yes :: Probability Integer)
          effect_caused = (Evidence effect yes) :: Evidence Bool Integer

main :: IO ()
main = defaultMain myTestGroup

