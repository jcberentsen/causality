{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module PopulationTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Evidence
import Observations
import Model
import Population
import Likelyhood

population_test_group = $(testGroupGenerator)

prop_likelyhood_generates_evidence toss =
    (pick toss (Likelyhood "heads" p)) == (Evidence "heads" (P (toss < 0.5)))
    where
        p = P 0.5 :: Probability Float

case_select_alternatives =
    select toss (Alternatives [heads, tails])
    @?= [heads, not_tails]
    where
        toss = 0.1 :: Double
        heads = fact "heads" :: Evidence String Bool
        tails = fact "tails"
        not_tails = dual tails

case_a_coin_has_two_possibilities =
    generate_population 2 (Alternatively (Alternatives [heads, tails])) Ignorance
    @?= [ conclude [heads, dual tails]
        , conclude [dual heads, tails]
        ]
    where
        heads = fact "heads" :: Evidence String Bool
        tails = fact "tails"

prop_unlikely_evidence =
    (length (filter isFact synthetic_evidence)) < 10
    where
        how_many = 100
        synthetic_evidence = map picker (many_tosses how_many)
        picker = (flip pick) (Likelyhood "unlikely" low_prob) :: Float -> Evidence String Bool
        low_prob = P 0.01 :: Probability Float

case_combine = do
    combine [] @?= ([] :: [[Bool]])
    combine [["single"]] @?= [["single"]]
    combine [[True], []] @?= []
    combine [[], [True]] @?= []
    combine [[True], [False]] @?= [[True,False]]

case_rain_sprinkler_likelyhood_yields_major_wetness_population =
    do
        wet_count @?= 55
    where
        wet_count = population_count (truly "wet") conclusions
        model = AnyCause [truly "rain", truly "sprinklers"] $ truly "wet"
        conclusions = generate_population tosses (Likely rain_sprinklers_priors) model
        tosses = 10
        observation_count = tosses * tosses

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
