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

case_gravity_causes_falling = do evidence_for_falling @?= eval_cause gravity gravity_is_a_fact

gravity_is_a_fact :: Evidence String Int
gravity_is_a_fact = fact "gravity"

evidence_for_falling :: Evidence String Int
evidence_for_falling = fact "falling"

case_no_gravity_causes_no_falling = do eval_cause gravity universe_with_no_gravity @?= no_evidence_for_falling

universe_with_no_gravity :: Evidence String Int
universe_with_no_gravity = Evidence "gravity" no

no_evidence_for_falling :: Evidence String Int
no_evidence_for_falling = Evidence "falling" no

prop_evidence_for_cause_causally_yields_effect (causality@(Causality cause effect))
    = effect_caused  == (eval_cause causality (Evidence cause yes))
        where
            _types = (causality :: Causality Bool, yes :: Probability Integer)
            effect_caused = (Evidence effect yes) :: Evidence Bool Integer

-- Rain or sprinklers cause wetness
-- Causal distribution
-- rain   sprinklers      wet
-- --------------------------
-- no     no              no
-- no     yes             yes
-- yes    no              yes
-- yes    yes             yes
-- --------------------------

-- "porch is wet" claim, proposition?
-- "porch is wet" fact, evidence
-- "is porch wet" query, how is that different from claim, really?
rain_proposition = "it has rained"
raining = fact rain_proposition :: Evidence String Int

sprinklers_proposition = "it has rained"
sprinklers = fact sprinklers_proposition :: Evidence String Int

wet_proposition = "porch is wet"
wet = fact wet_proposition :: Evidence String Int

rain_causes_wetness :: Model String Int (Causality String)
rain_causes_wetness = Cause $ Causality rain_proposition wet_proposition

sprinklers_cause_wetness :: Model String Int (Causality String)
sprinklers_cause_wetness = Cause $ Causality sprinklers_proposition wet_proposition

wetness_model :: Model String Int (Causality String)
wetness_model = rain_causes_wetness

case_rain_causing_wet = do eval_model wetness_model raining @?= wet
case_sprinklers_causing_wet = do eval_model sprinklers_cause_wetness sprinklers @?= wet

main :: IO ()
main = defaultMain myTestGroup

-- Notes:
-- Example of an effect with two causes?
-- rain or sprinklers cause wet
--
-- -- do(nothing) is the counterfactual. Replace an evidence with nothing to see the difference
