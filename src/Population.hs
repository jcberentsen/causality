{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Population where
-- <http://crpit.com/confpapers/CRPITV16Allison.pdf>

import Model
import Evidence
import Observations
import Likelyhood

type Population name p = [Observations name p]

generate_population ::
    (Truthy p, Eq p, Eq name, Ord like, Fractional like, Ord name, Ord p) =>
    Int -> Potential name like p -> CausalModel name p -> Population name p

generate_population how_many_tosses potential model =
    map ((flip eval_causalmodel) model) scenarios
    where
        scenarios = case potential of
            Likely likelyhoods -> combine $ synthesize_evidence how_many_tosses likelyhoods
            Alternatively alts -> all_combinations alts

synthesize_evidence :: (Truthy c, Ord b, Eq c, Eq a, Fractional b) => Int -> [Likelyhood a b] -> [[Evidence a c]]
synthesize_evidence how_many priors =
    map (\prior -> map (\toss -> Likelyhood.pick toss prior) (many_tosses how_many)) priors

-- combine [[a,b...], [c,d...]] = [[a, c], [a, d], [a, ...], [b, c], [b, d], [b, ...], ]]
-- TODO can this be made simpler, readable, evidently correct?
combine :: [[a]] -> [[a]]
combine (as:[]) = map (\a -> [a]) as
combine ((a:as):bss) = map (a:) (combine bss) ++ (combine (as:bss))
combine _ = []

-- Perfectly distributed random numbers between 0 and 1. In a particular nonrandom order (sorted!) :D
-- Ex. many_tosses 2 -> [0.0, 1.0]; many_tosses 3 yields [0.0, 0.5, 1.0]
many_tosses :: Fractional a => Int -> [a]
many_tosses how_many =
    take how_many $ map (\n -> (fromIntegral n) / (fromIntegral how_many - 1)) $ [0..how_many]

summarizePopulation :: (Eq name, Eq p, Ord name, Ord p) => Population name p -> [(name, Double)]
summarizePopulation _obs = []

count :: Eq a => a -> [a] -> Int
count a as =
    length $ filter (==a) as

population_count :: (Eq name, Eq p, Ord name, Ord p) => Evidence name p -> Population name p -> Int
population_count e pop =
    count e $ concat (map observations_toList pop)
