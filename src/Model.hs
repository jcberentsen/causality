{-# LANGUAGE
      GADTs
    , TemplateHaskell
    #-}

module Model where
-- <http://crpit.com/confpapers/CRPITV16Allison.pdf>

import Evidence
import Observations

import Data.Aeson.TH

import qualified Data.Set as Set

data CausalModel name prob where
    Ignorance :: CausalModel name prob
    Evidently :: { _evidence :: [Evidence name prob] } -> CausalModel name prob
    Causally :: { _causer :: Evidence name prob -- Note: perhaps Conditional is better semantic name for causer
                , _effect :: Evidence name prob -- Consider Conditional as separate type from Evidence?
                } -> CausalModel name prob
    AnyCause :: { _causes :: [Evidence name prob]
                , _effect :: Evidence name prob
                } -> CausalModel name prob
    AllCause :: { _causes :: [Evidence name prob]
                , _effect :: Evidence name prob
                } -> CausalModel name prob

    Multiple :: { _causalities :: [CausalModel name prob] } -> CausalModel name prob

$(deriveToJSON defaultOptions ''CausalModel)

instance (Show name, Show prob) => Show (CausalModel name prob) where
    show Ignorance = "Ignorance"
    show (Evidently e) = "Evidently " ++ show e
    show (Causally c e) = "Causally " ++ show c ++ " -> " ++ show e
    show (AnyCause c e) = "AnyCause " ++ show c ++ " -> " ++ show e
    show (AllCause c e) = "AllCause " ++ show c ++ " -> " ++ show e
    show (Multiple c) = "Multiple " ++ show c

instance (Eq prob, Eq name) => Eq (CausalModel name prob) where
    (==) Ignorance Ignorance = True
    (==) (Evidently e1) (Evidently e2) = e1 == e2
    (==) (Causally c1 e1) (Causally c2 e2) = c1 == c2 && e1 == e2
    (==) (AnyCause c1 e1) (AnyCause c2 e2) = c1 == c2 && e1 == e2
    (==) (AllCause c1 e1) (AllCause c2 e2) = c1 == c2 && e1 == e2
    (==) (Multiple c1) (Multiple c2) = c1 == c2
    (==) _ _  = False

eval_causalmodel :: (Truthy prob, Eq prob, Eq name, Ord prob, Ord name) =>
    [Evidence name prob] -> CausalModel name prob -> Observations name prob

eval_causalmodel observations Ignorance = conclude observations
eval_causalmodel observations (Evidently { _evidence = e }) = conclude $ observations ++ e
eval_causalmodel observations (Causally { _causer=(Evidence c _), _effect=(Evidence e _) }) =
    join_observations $ [(conclude $ observations)] ++ map (eval_cause c e) observations

eval_causalmodel observations (AnyCause { _causes=cs, _effect=e }) =
    conclude $ observations ++ case intersectWithObservations cs observations of
        [] -> []
        observed_causes -> if anyEvidenceFor observed_causes then [e] else [dual e]

eval_causalmodel observations (AllCause { _causes=cs, _effect=e }) =
    conclude $ observations ++ if observations == cs then [e] else [dual e]

eval_causalmodel observations model@(Multiple { _causalities=cs }) =
    let conclusions = join_observations (map (eval_causalmodel observations) cs)
    in
        if conclusions == (conclude observations) then conclusions
        else eval_causalmodel (observations_toList conclusions) model

--any cause implies effect
--note this is different from all causes are necessary to cause effect
(∴) :: [Evidence name prob] -> Evidence name prob -> CausalModel name prob
(|>) :: [Evidence name prob] -> Evidence name prob -> CausalModel name prob
causes ∴ effect = AnyCause causes effect
(|>) = (∴)

-- evaluating a cause yields the effect only if there is evidence present (missing or irrelevant evidence yields nothing)
eval_cause :: (Eq name, Eq a, Truthy a, Ord name, Ord a) => name -> name -> Evidence name a -> Observations name a
eval_cause cause effect (Evidence evidence val)
    = if evidence == cause then conclude [Evidence effect val] else Set.empty

