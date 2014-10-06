{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module PopulationTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC

import Evidence
import Model
import Population

population_test_group = $(testGroupGenerator)

prop_ignorant_model_generates_no_population seed = Population.sample seed model == []
    where model = Ignorance :: CausalModel Bool Bool

prop_evidently_model_always_generates_fact seed = Population.sample seed fact_model == [ a_fact ]
    where fact_model = Evidently [a_fact]
          a_fact = fact "a fact" :: Evidence String Bool

prop_sampling_cause_generates_cause_and_effect seed
    = seed == yes ==>
        Population.sample seed causal_model == [ cause, effect ]
    where causal_model = Causally cause effect
          cause = fact "cause" :: Evidence String Bool
          effect = fact "effect" :: Evidence String Bool

prop_sampling_cause_with_false_evidence_yields_counterfacts seed
    = seed == no ==>
        Population.sample seed causal_model == [ void cause, void effect ]
    where causal_model = Causally cause effect
          cause = fact "cause" :: Evidence String Bool
          effect = fact "effect" :: Evidence String Bool

-- Populations. A causal system is a generator of populations. The input is generators of facts.
-- The output is population of facts.
