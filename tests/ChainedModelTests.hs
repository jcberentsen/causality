{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module ChainedModelTests where

import TestHarness ()

import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Model
import Evidence

chained_model_test_group = $(testGroupGenerator)

raining = fact "rain" :: Evidence String Bool
wet = fact "wet" :: Evidence String Bool
slippery = fact "slippery" :: Evidence String Bool

rain_causes_wet = Causally raining wet
wet_causes_slippery = Causally wet slippery
rain_causes_wet_causes_slippery = Multiple [rain_causes_wet, wet_causes_slippery]

case_given_wet_causing_spillery_we_cant_conclude_slippery_from_rain = eval_causalmodel [raining] wet_causes_slippery @?= (conclude $ [raining, NoEvidence "slippery"])

case_rain_causes_wet_causes_slippery = eval_causalmodel (observations_toList (eval_causalmodel [raining] model)) model  @?= (conclude $ [raining, wet, slippery])
    where model = rain_causes_wet_causes_slippery
