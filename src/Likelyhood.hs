{-# LANGUAGE
      GADTs
    , DeriveDataTypeable
    , DeriveGeneric
    , TemplateHaskell
    #-}


module Likelyhood where

import Evidence
import Data.List
import Data.Typeable
import GHC.Generics
import Data.Aeson.TH

-- Likelyhood isn't evidence! It can generate distributions of evidence or be derived from populations of Evidence
data Likelyhood name p =
    Likelyhood name (Probability p)
    deriving (Generic, Show, Typeable, Eq, Ord)

$(deriveJSON defaultOptions ''Likelyhood)

data Alternatives name r = Alternatives [Evidence name r]
    deriving (Generic, Show, Typeable, Eq, Ord)

$(deriveJSON defaultOptions ''Alternatives)

alternatively :: Evidence name r -> Alternatives name r -> Alternatives name r
alternatively alt (Alternatives alts) = Alternatives $ alt:alts

toggle_alternative :: (Eq r, Eq name) => Evidence name r -> Alternatives name r -> Alternatives name r
toggle_alternative alt (Alternatives alts) = if elem alt alts then Alternatives (delete alt alts) else Alternatives $ alt:alts

data Potential name p r =
      Likely [Likelyhood name p]
    | Alternatively (Alternatives name r)

likely :: Truthy a => name -> a -> Likelyhood name a
likely name p = Likelyhood name (P p)

-- fifty fifty, maximum entropy?
chaotic :: (Truthy a, Fractional a) => name -> Likelyhood name a
chaotic name = Likelyhood name (P (recip 2))

pick :: (Ord like, Eq name, Eq like, Eq r, Truthy r) => like -> Likelyhood name like -> Evidence name r
pick random (Likelyhood name (P like)) =
    Evidence name (P (if random <= like then yes else no))

-- Select from alternatives should yield mutually exclusive evidence
select :: (RealFrac random, Eq name, Eq r, Truthy r) => random -> Alternatives name r -> [Evidence name r]
select random (Alternatives alts) = map decide (zip alts [(0::Int)..])
    where
        index = truncate $ random * (fromRational (toRational (length alts)))
        decide (alt, idx) = if idx == index then alt else dual alt

all_combinations :: Truthy r => Alternatives name r -> [[Evidence name r]]
all_combinations (Alternatives alts) = map indicate [1..length alts]
    where
        indicate indicator = map (\(alt, index) -> if index == indicator then alt else dual alt) (zip alts [1..])
