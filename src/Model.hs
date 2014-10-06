{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Model where
-- <http://crpit.com/confpapers/CRPITV16Allison.pdf>

import Evidence

-- Causality: cause effect
-- Probability distribution for cause
-- cause   |   effect
-- ------------------
-- no      |   no
-- yes     |   yes
-- ------------------
data Causality name = Causality name name deriving (Show)

data CausalModel name prob where
    Ignorance :: CausalModel name prob
    Evidently :: { _evidence :: [Evidence name prob] } -> CausalModel name prob
    Causally :: { _causer :: Evidence name prob
                , _effect :: Evidence name prob
                } -> CausalModel name prob
    AnyCause :: { _causes :: [Evidence name prob]
                , _effect :: Evidence name prob
                } -> CausalModel name prob

instance (Show name, Show prob) => Show (CausalModel name prob) where
    show Ignorance = "Ignorance"
    show (Evidently e) = "Evidently " ++ show e
    show (Causally c e) = "Causally " ++ show c ++ " -> " ++ show e
    show (AnyCause c e) = "AnyCause " ++ show c ++ " -> " ++ show e

instance (Eq prob, Eq name) => Eq (CausalModel name prob) where
    (==) Ignorance Ignorance = True
    (==) (Evidently e1) (Evidently e2) = e1 == e2
    (==) (Causally c1 e1) (Causally c2 e2) = c1 == c2 && e1 == e2
    (==) (AnyCause c1 e1) (AnyCause c2 e2) = c1 == c2 && e1 == e2
    (==) _ _  = False

eval_causalmodel :: (Truthy prob, Eq prob, Eq name) =>
    CausalModel name prob -> [Evidence name prob] -> [Evidence name prob]

eval_causalmodel Ignorance observations = observations
eval_causalmodel Evidently { _evidence = e } observations = observations ++ e
eval_causalmodel Causally { _causer=(Evidence c _), _effect=(Evidence e _) } observations =
    observations ++ map (eval_cause causality) observations
        where
            causality = Causality c e

eval_causalmodel AnyCause { _causes=cs, _effect=e } observations =
    observations ++ case intersectWithObservations cs observations of
        [] -> []
        observed_causes -> if anyEvidenceFor observed_causes then [e] else [dual e]

--any cause implies effect
--note this is different from all causes are necessary to cause effect
(∴) :: [Evidence name prob] -> Evidence name prob -> CausalModel name prob
(|>) :: [Evidence name prob] -> Evidence name prob -> CausalModel name prob
causes ∴ effect = AnyCause causes effect
(|>) = (∴)

eval_cause :: (Eq name, Eq a, Truthy a) => Causality name -> Evidence name a -> Evidence name a
eval_cause (Causality cause effect) (Evidence evidence val)
    = if evidence == cause then (Evidence effect val) else (no_evidence_for effect)

