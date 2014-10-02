{-# LANGUAGE EmptyDataDecls #-}

module Model where
-- <http://crpit.com/confpapers/CRPITV16Allison.pdf>

data Probability a = P a deriving (Show)
data Sure = Sure deriving (Show)

data Evidence name a = Evidence name (Probability a) deriving (Show)
data Fact name = Fact (Evidence name Sure) deriving (Show)

fact :: name -> Fact name
fact name = Fact (Evidence name (P Sure))

isFact :: Fact name -> Bool
isFact = const True

data Implication name = Implication (Fact name) (Fact name) deriving (Show)

eval :: Implication name -> Fact name
eval (Implication _premise conclusion) = conclusion

