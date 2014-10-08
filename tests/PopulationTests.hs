{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module PopulationTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
--import Test.Tasty.HUnit

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

-- TODO synthetic evidence should come in tuples
{-
-- case_rain_sprinkler_likelyhood_yields_major_wetness_population =
-- use likelyhood to generate synthetic evidence (observation)
-- run each observation through model to generate conclusions (effect)
-- collect all conclusions of type "wet" and count them
-- calculate ratio of "wet" against "not wet" it should correspond to causal model

    synthetic_evidence @?= [truly "rain", untruly "rain"]
    where
        synthetic_evidence = synthesize_evidence 10 rain_sprinklers_priors :: [Evidence String Bool]

rain_sprinklers_priors = [Likelyhood "rain" (P 0.5), Likelyhood "sprinklers" (P 0.1)] :: [Likelyhood String Float]

synthesize_evidence :: (Truthy c, Ord b, Eq c, Eq a, Fractional b) => Int -> [Likelyhood a b] -> [Evidence a c]
synthesize_evidence how_many priors =
    concat $ map (\prior -> map (\toss -> Population.select prior toss) (many_tosses how_many)) priors
-}

-- Perfectly distributed random numbers between 0 and 1. In a particular nonrandom order (sorted!) :D
-- Ex. many_tosses 2 -> [0.0, 1.0]; many_tosses 3 yields [0.0, 0.5, 1.0]
many_tosses :: Fractional a => Int -> [a]
many_tosses how_many = take how_many $ map (\n -> (fromIntegral n) / (fromIntegral how_many - 1)) $ [0..how_many]

-- Populations. A causal system is a generator of populations. The input is generators of facts.
-- The output is population of facts.

-- Figure out what evidence is input
-- Define Likelyhood of Evidence: Ex: [likelyhood "rain" 0.5, likelyhood "sprinklers" 0.1]
-- sampling an Evidence Distribution yields concrete evidence -> [E "rain" yes, E "sprinklers" no]
