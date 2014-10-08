{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module CausalModelTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Model
import Evidence

causal_model_test_group = $(testGroupGenerator)

gravity = Causality "gravity" "falling"

case_gravity_causes_falling = do evidence_for_falling @?= eval_cause gravity gravity_is_a_fact

gravity_is_a_fact :: Evidence String Bool
gravity_is_a_fact = fact "gravity"

evidence_for_falling :: Evidence String Bool
evidence_for_falling = fact "falling"

case_no_gravity_causes_no_falling = do eval_cause gravity universe_with_no_gravity @?= no_evidence_for_falling

universe_with_no_gravity :: Evidence String Bool
universe_with_no_gravity = Evidence "gravity" no

no_evidence_for_falling :: Evidence String Bool
no_evidence_for_falling = Evidence "falling" no

prop_evidence_for_cause_yields_effect (causality@(Causality cause effect))
    = effect_caused  == (eval_cause causality (Evidence cause yes))
        where
            _types = (causality :: Causality String, yes :: Probability Bool)
            effect_caused = (Evidence effect yes) :: Evidence String Bool

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
raining = fact rain_proposition :: Evidence String Bool
not_raining = void raining

sprinklers_proposition = "sprinklers"
sprinklers = fact sprinklers_proposition :: Evidence String Bool
no_sprinklers = void sprinklers

wet_proposition = "porch is wet"
wet = fact wet_proposition :: Evidence String Bool
not_wet = void wet

-- multiple causality and evidencing

-- CausalModel
prop_ignorance_only_yield_original_evidence evidence =
    eval_causalmodel Ignorance evidence == evidence
        where _types = evidence :: [Evidence () Bool]

prop_evidently_model_yields_evidence evidence =
    eval_causalmodel (Evidently [raining]) evidence == evidence ++ [raining]
        where _types = evidence :: [Evidence String Bool]

prop_fact_causes_effect any_fact =
    isFact any_fact ==> eval_causalmodel (Causally any_fact raining) [any_fact] == any_fact : [raining]
        where _types = any_fact :: Evidence String Bool

prop_AnyCause_yields_effect_given_any_of_the_causal_facts any_fact =
    isFact any_fact ==> eval_causalmodel (AnyCause [any_fact] raining) [any_fact] == any_fact : [raining]
        where _types = any_fact :: Evidence String Bool

prop_AnyCause_causes_nothing_given_no_facts cause =
    isFact cause ==> eval_causalmodel (AnyCause [cause] raining) [] == []
        where _types = cause :: Evidence String Bool

rain_or_sprinklers_cause_wetness = AnyCause [raining, sprinklers] wet

evidence_of_rain_and_sprinklers = [raining, sprinklers]
case_rain_or_sprinklers_causes_wetness =
    do eval_causalmodel rain_or_sprinklers_cause_wetness evidence_of_rain_and_sprinklers
       @?= evidence_of_rain_and_sprinklers ++ [wet]

case_no_evidence_cause_no_effect =
    do eval_causalmodel rain_or_sprinklers_cause_wetness [] @?= []

case_irrelevant_evidence_does_not_cause_rain =
    do eval_causalmodel rain_or_sprinklers_cause_wetness irrelevant_evidence @?= irrelevant_evidence
    where irrelevant_evidence = [fact "irrelevant"]

prop_evidence_contradicts_counter_evidence evidence =
    contradicting evidence (dual evidence)
        where _types = evidence :: Evidence Bool Bool

case_no_rain_nor_sprinklers_cause_not_wet =
    do eval_causalmodel rain_or_sprinklers_cause_wetness observations @?= observations ++ [not_wet]
        where observations = [not_raining, no_sprinklers]

prop_operator_model (cause1, cause2, effect) =
    cause1 <|> cause2 |> effect ==  AnyCause [cause1, cause2] effect
        where _types = [cause1, cause2, effect] :: [Evidence String Bool]

-- AllCause
prop_AllCause_causes_effect_given_all_facts facts =
        elem raining $ eval_causalmodel (AllCause facts raining) facts
        where _types = facts :: [Evidence String Bool]
