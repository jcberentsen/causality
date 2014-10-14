{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

-- cabal test --ghc-option=-v0
-- cabal configure --enable-tests && cabal build v0 && dist/build/causality-tests/causality-tests

import Test.Tasty

import PopulationTests
import CausalModelTests
import MonohybridCrossTests
import DihybridCrossTests
import ChainedModelTests
import JoinModelTests

all_tests =
    testGroup "All"
        [ causal_model_test_group
        , population_test_group
        , monohybrid_test_group
        , dihybrid_test_group
        , chained_model_test_group
        , join_model_test_group
        ]

main :: IO ()
main = defaultMain all_tests

-- -- do(nothing) is the counterfactual (What if we don't?) Replace evidence with nothing to see the difference
