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

generate_population ::
    (Truthy p, Eq p, Eq name, Ord like, Fractional like) =>
    [Likelyhood name like] -> CausalModel name p -> [[Evidence name p]]

generate_population potentials model =
    map (eval_causalmodel model) scenarios
    where
        synthetic_evidence = synthesize_evidence 2 potentials
        scenarios = combine synthetic_evidence

synthesize_evidence :: (Truthy c, Ord b, Eq c, Eq a, Fractional b) => Int -> [Likelyhood a b] -> [[Evidence a c]]
synthesize_evidence how_many priors =
    map (\prior -> map (\toss -> Population.select prior toss) (many_tosses how_many)) priors

-- combine [[a,b...], [c,d...]] = [[a, c], [a, d], [a, ...], [b, c], [b, d], [b, ...], ]]
-- TODO can this be made simpler, readable, evidently correct?
combine :: [[a]] -> [[a]]
combine (as:[]) = map (\a -> [a]) as
combine ((a:as):bss) = map (a:) (combine bss) ++ (combine (as:bss))
combine _ = []

-- Perfectly distributed random numbers between 0 and 1. In a particular nonrandom order (sorted!) :D
-- Ex. many_tosses 2 -> [0.0, 1.0]; many_tosses 3 yields [0.0, 0.5, 1.0]
many_tosses :: Fractional a => Int -> [a]
many_tosses how_many = take how_many $ map (\n -> (fromIntegral n) / (fromIntegral how_many - 1)) $ [0..how_many]

count :: Eq a => a -> [a] -> Int
count a as = length $ filter (==a) as
