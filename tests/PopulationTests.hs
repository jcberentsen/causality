{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module PopulationTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC

import Evidence
import Model
import Population
import Likelyhood

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

prop_likelyhood_generates_evidence toss =
    (Population.select (Likelyhood "heads" p) toss) == (Evidence "heads" (P (toss < 0.5)))
    where
        p = P 0.5 :: Probability Float

prop_unlikely_evidence =
    (length (filter isFact synthethic_evidence)) < 10
    where
        how_many = 100
        synthethic_evidence = map likelyhood (many_tosses how_many)
        likelyhood = Population.select (Likelyhood "unlikely" low_prob) :: Float -> Evidence String Bool
        low_prob = P 0.01 :: Probability Float

-- Perfectly distributed random numbers between 0 and 1. In a particular random order (sorted!) :D
many_tosses :: Fractional a => Int -> [a]
many_tosses how_many = take how_many $ map (\n -> (fromIntegral n) / (fromIntegral how_many)) $ [0::Int ..] 

-- Populations. A causal system is a generator of populations. The input is generators of facts.
-- The output is population of facts.
--
-- Figure out what evidence is input
-- Define Likelyhood of Evidence: Ex: [likelyhood "rain" 0.5, likelyhood "sprinklers" 0.1]
-- sampling an Evidence Distribution yields concrete evidence -> [E "rain" yes, E "sprinklers" no]
