{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Population where
-- <http://crpit.com/confpapers/CRPITV16Allison.pdf>

import Model
import Evidence

sample :: (Eq name, Eq p) => p -> CausalModel name p -> [Evidence name p]
sample _seed _model = []
