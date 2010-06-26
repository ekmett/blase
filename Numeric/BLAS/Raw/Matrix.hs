module Numeric.BLAS.Raw.Matrix
    ( Matrix
    , MMatrix
    , Mat
    , stride
    ) where

import Numeric.BLAS.Raw.Form
import Numeric.BLAS.Raw.Array

type Dense m n = Raw (GE m n) Pure
type MDense m n s = Raw (GE m n) (ST s)

type Banded m n ku kl = Raw (GB m n ku kl) Pure
type MBanded m n ku kl s = Raw (GB m n ku kl) (ST s)

type instance Rows (Raw t v a) = Raw t
type instance Rows (GE m n) = m 
type instance Rows (GB m n ku kl) = m

type family Columns a
type instance Columns (Raw t v a) = Columns t
type instance Columns (GE m n) = n
type instance Columns (GB m n ku kl) = n

data GE m n -- general dense vector
data GB m n ku kl -- general banded


-- BLAS vectors can have 'wide strides' to allow you to work with columns or rows as vectors.

instance Form Arr v a => Form (GE n m) v a where
    {-# SPECIALIZE instance Form (GE n m) Pure Float #-}
    {-# SPECIALIZE instance Form (GE n m) Pure Double #-}
    {-# SPECIALIZE instance Form (GE n m) Pure (Complex Float) #-}
    {-# SPECIALIZE instance Form (GE n m) Pure (Complex Double) #-}
    {-# SPECIALIZE instance Form (GE n m) (ST s) Float #-}
    {-# SPECIALIZE instance Form (GE n m) (ST s) Double #-}
    {-# SPECIALIZE instance Form (GE n m) (ST s) (Complex Float) #-}
    {-# SPECIALIZE instance Form (GE n m) (ST s) (Complex Double) #-}
    data Raw (GE n m) v a = GE
        {-# UNPACK #-} !Int     -- offset
        {-# UNPACK #-} !(Known Int n) -- columns
        {-# UNPACK #-} !(Known Int m) -- rows
        {-# UNPACK #-} !Int     -- stride
        {-# UNPACK #-} !Char    -- trans : 'N', 'T', 'H'
        !(Raw Arr v a)
    size (GE _ (Nat n) (Nat m) _ _) = fromNat n * fromNat m
    -- TODO: be smarter about this.
    aliases (GE _ _ _ _ _ a) (GE _ _ _ _ _ b) = aliases a b
    
-- stride :: Form (GE n m) v a => Raw (GE n m) v a -> Int
-- stride (GE _ _ _ n _ _) = n

instance Input Arr v a => Input (GE n m) v a where
    {-# SPECIALIZE instance Input (GE n m) Pure Float #-}
    {-# SPECIALIZE instance Input (GE n m) Pure Double #-}
    {-# SPECIALIZE instance Input (GE n m) Pure (Complex Float) #-}
    {-# SPECIALIZE instance Input (GE n m) Pure (Complex Double) #-}
    {-# SPECIALIZE instance Input (GE n m) (ST s) Float #-}
    {-# SPECIALIZE instance Input (GE n m) (ST s) Double #-}
    {-# SPECIALIZE instance Input (GE n m) (ST s) (Complex Float) #-}
    {-# SPECIALIZE instance Input (GE n m) (ST s) (Complex Double) #-}
    unsafeRead (GE b _ m s 'N' a) i = unsafeRead a (b + r + q * s)
            (q,r) = i `divMod` m 
    -- how to index other transpositions?
    unsafeFreeze (GE b n m s t a) = GE b n m s t `liftM` unsafeFreeze a

instance (Auto Int n, Auto Int m, Output Arr s a) => Output (GE n m) s a where
    {-# SPECIALIZE instance Output (GE n m) s Float #-}
    {-# SPECIALIZE instance Output (GE n m) s Double #-}
    {-# SPECIALIZE instance Output (GE n m) s (Complex Float) #-}
    {-# SPECIALIZE instance Output (GE n m) s (Complex Double) #-}
    -- unsafeNew ?
    new = GE 0 n m (known m) 'N' (unsafeNew (known n * known m))
        where
            n = auto
            m = auto
    unsafeWrite (GE b n m s t a) b c s a) i e = unsafeWrite a (b + r + q * s) e
        where
            (q,r) = i `divMod` m

class Square t n | t -> n  where
    diagonal :: Raw t v a -> Raw (Vec n) v a 

instance Form Arr v a => Square (GE n n) n v a where
    diagonal (GE b (Known n) (Known m) s c r) = 
        | c == 'H' = conjugate result
        | otherwise = result -- no copying needed
        where result = Vec b (min n m) (s + 1) r 

instance Form Arr v a => Square (GB n n ku kl) n v a where
    diagonal (GB b (Known n) (Known m) (Known ku) (Known kl) s c r = 
        undefined -- TODO
