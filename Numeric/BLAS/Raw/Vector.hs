module Numeric.BLAS.Raw.Vector
    ( Vector
    , MVector
    , Vec
    , stride
    ) where

import Numeric.BLAS.Raw.Form
import Numeric.BLAS.Raw.Array
import Numeric.BLAS.Raw.Known

type Vector n = Raw (Vec n) Pure
type MVector n s = Raw (Vec n) (ST s)

data Vec n

-- BLAS vectors can have 'wide strides' to allow you to work with columns or rows as vectors.

instance Form Arr v a => Form (Vec n) v a where
    {-# SPECIALIZE instance Form Vec Pure Float #-}
    {-# SPECIALIZE instance Form Vec Pure Double #-}
    {-# SPECIALIZE instance Form Vec Pure (Complex Float) #-}
    {-# SPECIALIZE instance Form Vec Pure (Complex Double) #-}
    {-# SPECIALIZE instance Form Vec (ST s) Float #-}
    {-# SPECIALIZE instance Form Vec (ST s) Double #-}
    {-# SPECIALIZE instance Form Vec (ST s) (Complex Float) #-}
    {-# SPECIALIZE instance Form Vec (ST s) (Complex Double) #-}
    data Raw (Vec n) v a = Vec
        {-# UNPACK #-} !Int -- offset
        {-# UNPACK #-} !(Known Int n) -- count
        {-# UNPACK #-} !Int -- stride
        !(Raw Arr v a)
    size (Vec _ (Known n) _ _) = n
    -- TODO: be smarter about this.
    aliases (Vec _ _ _ a) (Vec _ _ _ b) = aliases a b
    
stride :: Form (Vec n) v a => Raw (Vec n) v a -> Int
stride (Vec _ _ n _) = n

instance Input Arr v a => Input (Vec n) v a where
    {-# SPECIALIZE instance Input Vec Pure Float #-}
    {-# SPECIALIZE instance Input Vec Pure Double #-}
    {-# SPECIALIZE instance Input Vec Pure (Complex Float) #-}
    {-# SPECIALIZE instance Input Vec Pure (Complex Double) #-}
    {-# SPECIALIZE instance Input Vec (ST s) Float #-}
    {-# SPECIALIZE instance Input Vec (ST s) Double #-}
    {-# SPECIALIZE instance Input Vec (ST s) (Complex Float) #-}
    {-# SPECIALIZE instance Input Vec (ST s) (Complex Double) #-}
    unsafeRead (Vec b c s a) i = unsafeRead a (b + i * s)
    unsafeFreeze (Vec b c s a) = Vec b c s (unsafeFreeze a)

instance (Auto Int n, Output Arr s a) => Output (Vec n) s a where
    {-# SPECIALIZE instance Output Vec s Float #-}
    {-# SPECIALIZE instance Output Vec s Double #-}
    {-# SPECIALIZE instance Output Vec s (Complex Float) #-}
    {-# SPECIALIZE instance Output Vec s (Complex Double) #-}
    new = Vec 0 n 1 (unsafeNew (known n)) 
        where
            n = auto
    unsafeWrite (Vec b c s a) i e = unsafeWrite a (b + i * s) e

-- need instances for Show and Eq as well

{-
instance (Output (Vec n) s a, Auto Int n) => Num (Raw (Vec n) Pure a)
    fromIntegral e = runST $ do
        w <- new 
        forM [0..size w - 1] $ \i -> unsafeWrite w i (fromIntegral e)
        unsafeFreeze w
    (+) = liftV2 (+) 
    (-) = liftV2 (-)
    (*) = liftV2 (*)  -- elementwise multiplication. probably not what you intended, but hey
    abs = liftV1 abs
    signum = liftV1 signum

liftV1 :: (a -> a) -> Vector n a -> Vector n a
liftV1 f (Vector _ n _ a) = runST $ do
        w <- unsafeNew n
        forM [0..n - 1] $ \i -> do
            a <- unsafeRead u i 
            unsafeWrite w i (abs a)
        unsafeFreeze w

liftV2 :: (a -> a -> a) -> Vector n a -> Vector n a -> Vector n a
-}
