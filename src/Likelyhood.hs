{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Likelyhood where

import Evidence

-- Likelyhood isn't evidence! It can generate distributions of evidence or be derived from populations of Evidence
data Likelyhood name p =
    Likelyhood name (Probability p)
    deriving (Show, Eq)

data Alternatives name r = Alternatives [Evidence name r]
    deriving (Show, Eq)

likely :: Truthy a => name -> a -> Likelyhood name a
likely name p = Likelyhood name (P p)

-- fifty fifty, maximum entropy?
chaotic :: (Truthy a, Fractional a) => name -> Likelyhood name a
chaotic name = Likelyhood name (P (recip 2))

pick :: (Ord like, Eq name, Eq like, Eq r, Truthy r) => like -> Likelyhood name like -> Evidence name r
pick random (Likelyhood name (P like)) =
    Evidence name (P (if random <= like then yes else no))

-- Select on alternative should yield mutually exclusive evidence
select :: (Ord random, Eq name, Eq r, Truthy r) => random -> Alternatives name r -> [Evidence name r]
select _random (Alternatives alts) = map decide alts
    where
        decide alt = alt
