{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Population where
-- <http://crpit.com/confpapers/CRPITV16Allison.pdf>

import Model
import Evidence
import Likelyhood

sample :: (Eq name, Eq p, Truthy p) => p -> CausalModel name p -> [Evidence name p]
sample seed model =
    eval_causalmodel model synthetic_evidence
    where
        synthetic_evidence =
            case model of
                Causally cause _ -> [if seed == yes then cause else void cause]
                _ -> []

select :: (Ord like, Eq name, Eq like, Eq r, Truthy r) => Likelyhood name like -> like -> Evidence name r
select (Likelyhood name (P like)) reality = Evidence name (P (if reality <= like then yes else no))
