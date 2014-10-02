{-# LANGUAGE EmptyDataDecls #-}

module Model where
-- <http://crpit.com/confpapers/CRPITV16Allison.pdf>

data Probability a = P a deriving (Show, Eq)

class Truthy a where
    truthy :: a -> Bool

instance Truthy Double where
    truthy v = if v < 0.96 then False else True

data Evidence name a = Evidence name (Probability a) deriving (Show, Eq)

yes :: Num a => Probability a
yes = P 1

no :: Num a => Probability a
no = P 0

fact :: Num a => name -> Evidence name a
fact name = Evidence name yes

no_evidence_for :: Num a => name -> Evidence name a
no_evidence_for name = Evidence name no

isFact :: Truthy a => Evidence name a -> Bool
isFact (Evidence _ (P v)) = truthy v

void :: Num a => Evidence name a -> Evidence name a
void (Evidence name (P p)) = Evidence name (P (p-p))

-- Causality: cause effect
-- Probability distribution for cause
-- cause   |   effect
-- ------------------
-- no      |   no
-- yes     |   yes
-- ------------------
data Causality name = Causality name name deriving (Show)

eval :: (Eq name, Eq a, Num a) => Causality name -> Evidence name a -> Evidence name a
eval (Causality cause effect) (Evidence evidence val) = if evidence == cause then (Evidence effect val) else (no_evidence_for effect)

