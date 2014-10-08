{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module PhenotypeTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC

import Evidence
import Model
--import Population
--import Likelyhood

monohybride_test_group = $(testGroupGenerator)

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

-- Positional alleles in genes
allele_B1 = truly "B1"
allele_B2 = truly "B2"

allele_b1 = truly "b1"
allele_b2 = truly "b2"

-- Phenotype facts
blue = truly "blue"
brown = truly "brown"

-- Gene causes Phenotype
cause_brown = AnyCause [allele_B] brown

prop_required_cause_for_brown_phenotype =
    elem brown $ eval_causalmodel cause_brown [allele_B]

prop_only_b_allele_not_enough_to_conclude_brown_phenotype =
    not (elem brown $ eval_causalmodel cause_brown [allele_b])
