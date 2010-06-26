module Numeric.BLAS.Raw.Known 
    ( Z, S, D, P
    , knownZero, knownSucc, knownDouble, knownPred
    , reifyKnown, reflectKnown, eq, cmp, unsafeKnown, known, nat, Auto(..)
    ) where

newtype Known a p = Known a

instance Eq (Known a p) where
    _ == _ = True

instance Ord (Known a p) where
    compare _ _ = EQ
 
data Z
knownZero :: Known Int Z
knownZero = Known 0

data S n
knownSucc :: Known Int n -> Known Int (S n)
knownSucc (Known !n) = Known (n + 1)

data P n
knownPred :: Known Int n -> Known Int (P n)
knownPred (Known !n) = Known (n - 1)

data D n
knownDouble :: Known Int n -> Known Int (D n)
knownDouble (Known !n) = Known (n * 2)

reifyKnown :: a -> (forall p. (Auto p, Value p ~ a) => Known a p -> r) -> r
reifyKnown n f = reify n (reflectKnown f)

reflectKnown :: Reifies s a => (Known a (Reified s a) -> r) -> Tagged s r
reflectKnown f = unsafeTaggedKnown f reflect

-- unsafe in general
unsafeTaggedKnown :: Reifies s a => (Known a (Reified s a) -> r) -> Tagged s a -> Tagged s r 
unsafeTaggedKnown f (Tagged a) = f (Known a)

eq :: Eq a => Known a n -> Known a m -> Bool
eq (Known n) (Known m) = n == m 

cmp :: Ord a => Known a n -> Known a m -> Ordering
cmp (Known n) (Known m) = compare n m

unsafeKnown :: Known a p -> Known a q
unsafeKnown (Known n) = Known m

known :: Known a p -> a
known (Known n) = n

class Auto p where
    type Value p 
    auto :: Known (Value p) p

data Reified s a
instance Reifies s a => Auto (Reified s a) where
    type Value (Reified s a) = a 
    auto = proxy reflect (Proxy :: Proxy s)

instance Auto Z where
    type Value Z = Int
    auto = zero

instance Auto Int n => Auto Int (S n) where
    type Value (S n) = Int
    auto = succ auto

instance Auto Int n => Auto Int (D n) where
    type Value (D n) = Int
    auto = double auto

instance Auto Int n -> Auto Int (P n) where
    auto = pred auto

nat :: Int -> Q Exp
nat i = case quotRem i 2 of
    (0,0)  -> conT ''Z
    (j,0)  -> conT ''D `appT` int j
    (j,1)  -> conT ''S `appT` (conT ''D `appT` int j)
    -- (j,-1) -> conT ''P `appT` (conT ''D `appT` int j)
