{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module PopulationTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC

import Model
import Population

population_test_group = $(testGroupGenerator)

prop_ignorant_model_generates_no_population seed = Population.sample seed model == []
    where model = Ignorance :: CausalModel Bool Bool

-- Populations. A causal system is a generator of populations. The input is generators of facts.
-- The output is population of facts.
