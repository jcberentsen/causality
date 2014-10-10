{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module PopulationTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

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
    (length (filter isFact synthetic_evidence)) < 10
    where
        how_many = 100
        synthetic_evidence = map likelyhood (many_tosses how_many)
        likelyhood = Population.select (Likelyhood "unlikely" low_prob) :: Float -> Evidence String Bool
        low_prob = P 0.01 :: Probability Float

case_combine = do
    combine [] @?= ([] :: [[Bool]])
    combine [["single"]] @?= [["single"]]
    combine [[True], []] @?= []
    combine [[], [True]] @?= []
    combine [[True], [False]] @?= [[True,False]]

case_rain_sprinkler_likelyhood_yields_major_wetness_population =
    do
         count (truly "wet") (concat conclusions) @?= 3
         count (untruly "wet") (concat conclusions) @?= 1
    where
        model = AnyCause [truly "rain", truly "sprinklers"] $ truly "wet"
        conclusions = generate_population rain_sprinklers_priors model

case_rain_sprinkler_likely_synthetic_population =
    synthetic_evidence @?=
        [ [truly "rain", truly "sprinklers"]
        , [truly "rain", untruly "sprinklers"]
        , [untruly "rain", truly "sprinklers"]
        , [untruly "rain", untruly "sprinklers"]
        ]
    where
        synthetic_evidence = combine $ synthesize_evidence 2 rain_sprinklers_priors :: [[Evidence String Bool]]

rain_sprinklers_priors = [Likelyhood "rain" (P 0.5), Likelyhood "sprinklers" (P 0.1)] :: [Likelyhood String Float]

-- Populations. A causal system is a generator of populations. The input is generators of facts.
-- The output is population of facts.

-- Figure out what evidence is input
-- Define Likelyhood of Evidence: Ex: [likelyhood "rain" 0.5, likelyhood "sprinklers" 0.1]
-- sampling an Evidence Distribution yields concrete evidence -> [E "rain" yes, E "sprinklers" no]
