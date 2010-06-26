{-# LANGUAGE MagicHash, MultiParamTypeClasses, TypeFamilies #-}
module Numeric.BLAS.Raw.Arr 
    ( Arr
    , MArr
    , Arr
    , Primitive(..)
    , PrimitiveM(..)
    ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive.ByteArray
import Data.Primitive.Types
import GHC.Prim
import GHC.Int
import Numeric.BLAS.Raw.Form

type Array = Raw Arr Pure
type MArray s = Raw Arr (ST s)

data Arr

sIZEOF_DOUBLE, sIZEOF_FLOAT : Int 
sIZEOF_DOUBLE = I# (sizeOf# (undefined :: Double))
sIZEOF_FLOAT = I# (sizeOf# (undefined :: Double))

instance Form Arr Pure Float where
    newtype Raw Arr Pure Float = ArrF ByteArray
    size (ArrF s) = Prim.size s `div` sIZEOF_FLOAT
    aliases _ _ = False

instance Form Arr Pure Double where
    newtype Raw Arr Pure Double = ArrD ByteArray
    size (ArrD s) = Prim.size s `div` sIZEOF_DOUBLE
    aliases _ _ = False

instance Form Arr (ST s) Float where
    newtype Raw Arr (ST s) Float = STArrF (MutableByteArray s)
    size (STArrF s) = Prim.size s `div`  sIZEOF_FLOAT
    aliases (STArrF a) (STArrF b) = sameMutableByteArray a b

instance Form Arr (ST s) Double where
    newtype Raw Arr (ST s) Float = STArrD (MutableByteArray s)
    size (STArrD s) = Prim.size s `div`  sIZEOF_DOUBLE
    aliases (STArrD a) (STArrD b) = sameMutableByteArray a b

instance Form Arr v a => Form Arr v (Complex a) where
    {-# SPECIALIZE instance Form Arr Pure (Complex Float) #-}
    {-# SPECIALIZE instance Form Arr Pure (Complex Double) #-}
    {-# SPECIALIZE instance Form Arr (ST s) (Complex Float) #-}
    {-# SPECIALIZE instance Form Arr (ST s) (Complex Double) #-}
    newtype Raw Arr v (Complex a) = ArrC (Arr v a)
    size (ArrC a) = size a `div` 2
    aliases (ArrC a) (ArrC b) = aliases a b

instance Input Arr Pure s Double where
    read (ArrD a) i = return $ indexByteArray a i 
    unsafeFreeze = return
    freeze = return

instance Input Arr Pure s Float where
    read (ArrF a) i = return $ indexByteArray a i 
    unsafeFreeze = return
    freeze = return

instance Input Arr (ST s) s Double where
    read (STArrD a) i = readByteArray a i
    unsafeFreeze (STArrD a) = STArrD `liftM` unsafeFreezeByteArray a

instance Input Arr (ST s) s Float where
    read (STArrF a) i = readByteArray a i
    unsafeFreeze = liftM ArrC . unsafeFreeze . getArrC

instance (Input Arr v s a, ReadFloat a) => Input Arr v s (Complex a) where
    {-# SPECIALIZE instance Input Arr Pure s (Complex Float) #-}
    {-# SPECIALIZE instance Input Arr Pure s (Complex Double) #-}
    {-# SPECIALIZE instance Input Arr (ST s) s (Complex Float) #-}
    {-# SPECIALIZE instance Input Arr (ST s) s (Complex Double) #-}
    read (ArrC a) ((*2) -> i2) = do
        x <- read a i2
        y <- read a (i2 + 1)
        return (x :+ y)
    unsafeFreeze (STArrF a) = STArrF `liftM` unsafeFreezeByteArray a

instance Output Arr s Double where
    new i = newByteArray (i * sIZEOF_DOUBLE)
    write (STArrD a) i e = writeByteArray a i e

instance Output Arr s Float where
    new i = newByteArray (i * sIZEOF_FLOAT)
    write (STArrF a) i e = writeByteArray a i e

instance (Output Arr s a, RealFloat a) => Output Arr s (Complex a) where
    {-# SPECIALIZE instance Output Arr s (Complex Float) #-}
    {-# SPECIALIZE instance Output Arr s (Complex Double) #-}
    new i = ArrC `liftM` new (i * 2)
    write (ArrC a) ((*2) -> i2) (x :+ y) = do
        writeByteArray a i2 x
        writeByteArray a (i2 + 1) y

class Primitive a where
    toPrimitiveVector  :: Raw Arr Pure a   -> Prim.Vector a

class PrimitiveM s a where
    toPrimitiveMVector :: Raw Arr (ST s) a -> Prim.MVector s a

instance Primitive Float where
    toPrimitiveVector a@(ArrF ba)     = Prim.Vector 0 (size a) ba
instance PrimitiveM Float
    toPrimitiveMVector a@(STArrF mba) = Prim.Vector 0 (size a) mba

instance Primitive Double where
    toPrimitiveVector a@(ArrD ba)     = Prim.Vector 0 (size a) ba
instance PrimitiveM Double where
    toPrimitiveMVector a@(STArrD mba) = Prim.Vector 0 (size a) mba

-- | NB: currently aliasing is lost if we use 'tupled' or 'untupled'.
-- We should expose a deep content type using 'Dynamic', and compare for equality on that

tupled :: (Form Arr v a, RealFloat a) => Raw Arr v a -> Raw Arr v (Complex a)
tupled = ArrC

untupled :: (Form Arr v a, RealFloat a) => Raw Arr v (Complex a) -> Raw Arr v a
untupled (ArrC a) = a
