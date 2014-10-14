{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module JoinModelTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Model
import Evidence
import Observations

import qualified Data.Set as Set

join_model_test_group = $(testGroupGenerator)

rain :: Evidence String Bool
rain = fact "rain"

wet :: Evidence String Bool
wet = fact "wet"

rain_causes_wet :: CausalModel String Bool
rain_causes_wet = Causally rain wet

evidently_raining = Evidently [rain]
evidently_raining_and_wet = Evidently [rain, wet]

case_joining_rain_and_rain_causing_wetness_yields_evidently_both =
    join_models
        rain_causes_wet
        evidently_raining
    @?= evidently_raining_and_wet
