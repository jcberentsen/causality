{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Population where
-- <http://crpit.com/confpapers/CRPITV16Allison.pdf>

import Model

sample :: (Eq name, Eq p) => Int -> CausalModel name p -> [Evidence name p]
sample _seed _model = []
