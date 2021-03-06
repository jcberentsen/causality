{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module MonohybridCrossTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Evidence
import Model
import Population
import Likelyhood

import qualified Data.Set as Set

monohybrid_test_group = $(testGroupGenerator)

-- Alleles B (brown), b (blue)
-- B dominates  b is recessive

-- Genotype Phenotype
-- bb     blue
-- Bb     brown
-- bB     brown
-- BB     brown

-- Mother Father  Possible Gametes
-- bb     bb      bb
-- Bb     bB      BB, bB, Bb, bb

-- Allele facts
allele_B = truly "B"
allele_b = truly "b"

-- Phenotype facts
blue = truly "blue"
brown = truly "brown"

-- Gene causes Phenotype
cause_brown = AnyCause [allele_B] brown

prop_required_cause_for_brown_phenotype =
    Set.member brown $ eval_causalmodel [allele_B] cause_brown

prop_only_b_allele_not_enough_to_conclude_brown_phenotype =
    not (Set.member brown $ eval_causalmodel [allele_b] cause_brown)

case_synthetic_evidence = do
    combinations @?=
        [ [truly "B",   truly "b"]
        , [truly "B",   untruly "b"]
        , [untruly "B", truly "b"]
        , [untruly "B", untruly "b"]
        ]
    where
        combinations = combine $ synthesize_evidence 2 priors
        priors = [chaotic "B", chaotic "b"] :: [Likelyhood String Double]

case_when_random_occurences_of_allele_B_and_b_half_population_is_brown = do
    -- allele_B explains it
    population_count (truly "brown") crossings @?= 2
    where
        crossings = generate_population 2 priors cause_brown
        priors = Likely [chaotic "B", chaotic "b"] :: Potential String Double Bool
        -- Idea: forget :: Evidence name a -> Likelyhood name b
        -- or some other name for replacing evidence with likelyhood

