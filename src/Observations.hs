{-# LANGUAGE
      DeriveDataTypeable
    , DeriveGeneric
    , TemplateHaskell
    #-}

module Observations where

import Evidence
import Data.Set (Set)
import qualified Data.Set as Set

type Observations name p = Set (Evidence name p)

conclude :: (Ord p, Ord name) => [Evidence name p] -> Observations name p
conclude ev = Set.fromList ev

join_observations :: (Ord name, Ord p) => [Observations name p] -> Observations name p
join_observations obs = Set.unions obs

observations_toList :: Observations name p -> [Evidence name p]
observations_toList obs = Set.toList obs

