{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module DihybridCrossTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC

import Examples.DihybridCross

import Model

import qualified Data.Set as Set

dihybrid_test_group = $(testGroupGenerator)

prop_required_cause_for_green_phenotype =
    Set.member green $ eval_causalmodel [allele_G, both_bb] cause_green

prop_only_G_allele_not_enough_to_conclude_green_phenotype =
    not (Set.member green $ eval_causalmodel [allele_G] cause_green)
