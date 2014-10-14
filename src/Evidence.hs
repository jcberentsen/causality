{-# LANGUAGE
      EmptyDataDecls
    , GADTs
    , DeriveDataTypeable
    , DeriveGeneric
    , EmptyDataDecls
    , FlexibleContexts
    , FlexibleInstances
    , ScopedTypeVariables
    , StandaloneDeriving
    , TemplateHaskell
    , TupleSections
    , TypeFamilies
    , UndecidableInstances
    #-}

module Evidence where

import Data.List
import Data.Typeable
import GHC.Generics
import Data.Aeson.TH

import Data.Set (Set)
import qualified Data.Set as Set

data Probability p = P p
    deriving (Generic, Show, Typeable, Eq, Ord)
$(deriveJSON defaultOptions ''Probability)

class Truthy p where
    truthy :: p -> Bool
    yes :: p
    no :: p

instance Truthy Double where
    truthy v = if v < 0.96 then False else True
    yes = 1.0
    no = 0.0

instance Truthy Bool where
    truthy b = b
    yes = True
    no = False

instance Truthy p => Truthy (Probability p) where
    truthy (P p) = truthy p
    yes = P yes
    no = P no

data Evidence name p = Evidence name (Probability p) | NoEvidence name -- Note: consider using Maybe Evidence where appropriate?
    deriving (Generic, Show, Typeable, Eq, Ord)

$(deriveJSON defaultOptions ''Evidence)

type Observations name p = Set (Evidence name p)

conclude :: (Ord p, Ord name) => [Evidence name p] -> Observations name p
conclude ev = Set.fromList ev

join_observations :: (Ord name, Ord p) => [Observations name p] -> Observations name p
join_observations obs = Set.unions obs

observations_toList :: Observations name p -> [Evidence name p]
observations_toList obs = Set.toList obs

anyEvidenceFor :: Truthy b => [Evidence a b] -> Bool
anyEvidenceFor = any (\(Evidence _ p) -> truthy p)

allEvidenceFor :: Truthy b => [Evidence a b] -> Bool
allEvidenceFor = all (\(Evidence _ p) -> truthy p)

intersectWithObservations :: Eq a => [Evidence a b] -> [Evidence a b] -> [Evidence a b]
intersectWithObservations claims observations =
    intersectBy (\(Evidence a _) (Evidence b _) -> a==b) observations claims -- observations are kept
    -- thus contradicting observations will end up as evidence

contradicting :: (Eq a, Eq b) => Evidence a b -> Evidence a b -> Bool
contradicting (Evidence a pa) (Evidence b pb) = if a == b then if pa /= pb then True else False else False
contradicting (Evidence a pa) (NoEvidence b) = False
contradicting (NoEvidence a) (Evidence b pb) = False
contradicting (NoEvidence a) (NoEvidence b) = False

-- or
(<|>) :: Evidence name prob -> Evidence name prob -> [Evidence name prob]
e1 <|> e2 = [e1, e2]

fact :: Truthy a => name -> Evidence name a
fact name = Evidence name yes

truly :: name -> Evidence name Bool
truly name = fact name

untruly :: name -> Evidence name Bool
untruly name = void $ fact name

counterfact :: Truthy a => name -> Evidence name a
counterfact name = Evidence name no

isFact :: Truthy a => Evidence name a -> Bool
isFact (Evidence _ (P v)) = truthy v
isFact (NoEvidence _ ) = False

isEvidenced :: Evidence name a -> Bool
isEvidenced (Evidence _ _) = True
isEvidenced _ = False

void :: Truthy a => Evidence name a -> Evidence name a
void (Evidence name _) = Evidence name no
void (NoEvidence name) = Evidence name no

dual :: Truthy a => Evidence name a -> Evidence name a
dual (Evidence name p) = Evidence name (if truthy p then no else yes)
dual (NoEvidence name) = NoEvidence name
