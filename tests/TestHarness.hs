{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module TestHarness where

import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit ()
import Data.DeriveTH

import Evidence
import Model

-- derive arbitrary for code under test
derive makeArbitrary ''Probability
derive makeArbitrary ''Evidence

