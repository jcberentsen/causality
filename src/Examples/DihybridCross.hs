{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module Examples.DihybridCross where

import Evidence
import Model

-- Alleles B (brown), b (blue), G (green), g (offgreen)
-- B dominates G. b is always recessive

-- Genotype Phenotype
-- bbgg     blue
-- bbGg     green
-- bbGG     green
-- Bbgg     brown
-- BBgg     brown
-- BbGg     brown
-- BBGg     brown
-- BBGG     brown
-- BbGG     brown

-- Mother   Father    Possible Children
-- bbgg     bbgg      bbgg
-- BbGg     BbGg      BBGG, BbGg, etc...

-- Allele facts
allele_G = truly "G"
allele_g = truly "g"
allele_B = truly "B"
allele_b = truly "b"

-- Positional alleles in genes
allele_G1 = truly "G1"
allele_G2 = truly "G2"

allele_g1 = truly "g1"
allele_g2 = truly "g2"

allele_B1 = truly "B1"
allele_B2 = truly "B2"

allele_b1 = truly "b1"
allele_b2 = truly "b2"

-- derived facts from positional alleles
cause_G = AnyCause [allele_G1, allele_G2] allele_G

-- Phenotype facts
blue = truly "blue"
green = truly "green"
brown = truly "brown" :: Evidence String Bool

-- Gene causes Phenotype
devo_brown = AnyCause [allele_B] brown

-- green devo when 'all b' alleles (ie not any B) and any 'G'
cause_bb = AllCause [allele_b1, allele_b2] both_bb
both_bb = truly "bb"
cause_green = AllCause [allele_G, both_bb] green :: CausalModel String Bool

