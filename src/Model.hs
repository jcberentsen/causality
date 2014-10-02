{-# LANGUAGE EmptyDataDecls #-}

module Model where
-- <http://crpit.com/confpapers/CRPITV16Allison.pdf>
--
import Data.ByteString

foo :: ()
foo = undefined

type Ident = ByteString
data Fact = Fact Ident

isFact :: Fact -> Bool
isFact = const True

data Implication = Implication Fact Fact

eval :: Implication -> Fact
eval (Implication _premise conclusion) = conclusion

