{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Likelyhood where

import Evidence

-- Likelyhood isn't evidence! It can generate distributions of evidence or be derived from populations of Evidence
data Likelyhood name p = Likelyhood name (Probability p) deriving (Show, Eq)

likely :: Truthy a => name -> a -> Likelyhood name a
likely name p = Likelyhood name (P p)

-- fifty fifty, maximum entropy?
chaotic :: (Truthy a, Fractional a) => name -> Likelyhood name a
chaotic name = Likelyhood name (P (recip 2))

