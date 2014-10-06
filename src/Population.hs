{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Population where
-- <http://crpit.com/confpapers/CRPITV16Allison.pdf>

import Model
import Evidence

sample :: (Eq name, Eq p, Truthy p) => p -> CausalModel name p -> [Evidence name p]
sample seed model = case model of
    Ignorance -> []
    Evidently e -> e
    Causally cause effect -> eval_causalmodel model synthetic_evidence
        where
            synthetic_evidence = [if seed == yes then cause else void cause]
    _ -> []
