{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module CausalModelTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Model
import Evidence
import Observations

import qualified Data.Set as Set

causal_model_test_group = $(testGroupGenerator)

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
    eval_causalmodel evidence Ignorance == conclude evidence
        where _types = evidence :: [Evidence () Bool]

prop_evidently_model_yields_evidence evidence =
    eval_causalmodel evidence (Evidently [raining]) == (conclude $ evidence ++ [raining])
        where _types = evidence :: [Evidence String Bool]

prop_fact_causes_effect any_fact =
    isFact any_fact ==> eval_causalmodel [any_fact] (Causally any_fact raining) == (conclude $ any_fact : [raining])
        where _types = any_fact :: Evidence String Bool

prop_AnyCause_yields_effect_given_any_of_the_causal_facts any_fact =
    isFact any_fact ==> eval_causalmodel [any_fact] (AnyCause [any_fact] raining) == (conclude $ any_fact : [raining])
        where _types = any_fact :: Evidence String Bool

prop_AnyCause_causes_nothing_given_no_facts cause =
    isFact cause ==> eval_causalmodel [] (AnyCause [cause] raining) == (conclude [])
        where _types = cause :: Evidence String Bool

rain_or_sprinklers_cause_wetness = AnyCause [raining, sprinklers] wet

evidence_of_rain_and_sprinklers = [raining, sprinklers]
case_rain_or_sprinklers_causes_wetness =
    do eval_causalmodel evidence_of_rain_and_sprinklers rain_or_sprinklers_cause_wetness
       @?= (conclude $ evidence_of_rain_and_sprinklers ++ [wet])

case_no_evidence_cause_no_effect =
    do eval_causalmodel [] rain_or_sprinklers_cause_wetness @?= conclude []

case_irrelevant_evidence_does_not_cause_rain =
    do eval_causalmodel irrelevant_evidence rain_or_sprinklers_cause_wetness @?= (conclude irrelevant_evidence)
    where irrelevant_evidence = [fact "irrelevant"]

prop_evidence_contradicts_counter_evidence evidence =
    contradicting evidence (dual evidence)
        where _types = evidence :: Evidence Bool Bool

case_no_rain_nor_sprinklers_cause_not_wet =
    do eval_causalmodel observations rain_or_sprinklers_cause_wetness @?= (conclude $ observations ++ [not_wet])
        where observations = [not_raining, no_sprinklers]

prop_operator_model (cause1, cause2, effect) =
    cause1 <|> cause2 |> effect == AnyCause [cause1, cause2] effect
        where _types = [cause1, cause2, effect] :: [Evidence String Bool]

-- AllCause
prop_AllCause_causes_effect_given_all_facts facts =
        Set.member raining $ eval_causalmodel facts (AllCause facts raining)
        where _types = facts :: [Evidence String Bool]
