{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Model where
-- <http://crpit.com/confpapers/CRPITV16Allison.pdf>

import Data.List

data Probability a = P a deriving (Show, Eq)

class Truthy a where
    truthy :: a -> Bool
    yes :: a
    no :: a

instance Truthy Double where
    truthy v = if v < 0.96 then False else True
    yes = 1.0
    no = 0.0

instance Truthy Bool where
    truthy b = b
    yes = True
    no = False

instance Truthy a => Truthy (Probability a) where
    truthy (P p) = truthy p
    yes = P yes
    no = P no

data Evidence name a = Evidence name (Probability a) deriving (Show, Eq)

-- Causality: cause effect
-- Probability distribution for cause
-- cause   |   effect
-- ------------------
-- no      |   no
-- yes     |   yes
-- ------------------
data Causality name = Causality name name deriving (Show)

data CausalModel name prob term where
    Ignorance :: () -> CausalModel name prob ()
    Evidently :: { _evidence :: [Evidence name prob] } -> CausalModel name prob [Evidence name prob]
    Causally :: { _causer :: Evidence name prob
                , _effect :: Evidence name prob
                } -> CausalModel name prob (Evidence name prob)
    AnyCause :: { _causes :: [Evidence name prob]
                , _effect :: Evidence name prob
                } -> CausalModel name prob (Evidence name prob)

eval_causalmodel :: (Truthy prob, Eq prob, Eq name) =>
    CausalModel name prob term -> [Evidence name prob] -> [Evidence name prob]

eval_causalmodel (Ignorance _)  _observations = []
eval_causalmodel Evidently { _evidence = e } _observations = e -- no need to consider observations
eval_causalmodel Causally { _causer=(Evidence c _), _effect=(Evidence e _) } observations =
    map (eval_cause causality) observations
        where
            causality = Causality c e

eval_causalmodel AnyCause { _causes=cs, _effect=e } observations =
    case intersectWithObservations cs observations of
        [] -> []
        observed_causes -> if anyEvidenceFor observed_causes then [e] else [dual e]

anyEvidenceFor :: Truthy b => [Evidence a b] -> Bool
anyEvidenceFor = any (\(Evidence _ p) -> truthy p)

intersectWithObservations :: Eq a => [Evidence a b] -> [Evidence a b] -> [Evidence a b]
intersectWithObservations claims observations =
    intersectBy (\(Evidence a _) (Evidence b _) -> a==b) observations claims -- observations are kept
    -- thus contradicting observations will end up as evidence

contradicting :: (Eq a, Eq b) => Evidence a b -> Evidence a b -> Bool
contradicting (Evidence a pa) (Evidence b pb) = if a == b then if pa /= pb then True else False else False

data Model name prob term where
    Cause :: { _cause :: Causality name} -> Model name prob (Causality name)

-- or
(<|>) :: Evidence name prob -> Evidence name prob -> Evidence name prob
ev1 <|> _ = ev1

--implies
(∴) :: Evidence name prob -> Evidence name prob -> Causality name
(|>) :: Evidence name prob -> Evidence name prob -> Causality name
(Evidence cause _) ∴ (Evidence effect _) = Causality cause effect
(|>) = (∴)

eval_cause :: (Eq name, Eq a, Truthy a) => Causality name -> Evidence name a -> Evidence name a
eval_cause (Causality cause effect) (Evidence evidence val)
    = if evidence == cause then (Evidence effect val) else (no_evidence_for effect)

eval_model :: (Truthy prob, Eq prob, Eq name) => Model name prob a -> [Evidence name prob] -> [Evidence name prob]
eval_model Cause { _cause = c } evidence = map (eval_cause c) evidence

fact :: Truthy a => name -> Evidence name a
fact name = Evidence name yes

counterfact :: Truthy a => name -> Evidence name a
counterfact name = Evidence name no

no_evidence_for :: Truthy a => name -> Evidence name a
no_evidence_for name = Evidence name no

isFact :: Truthy a => Evidence name a -> Bool
isFact (Evidence _ (P v)) = truthy v

void :: Truthy a => Evidence name a -> Evidence name a
void (Evidence name _) = Evidence name no

dual :: Truthy a => Evidence name a -> Evidence name a
dual (Evidence name p) = Evidence name (if truthy p then no else yes)


