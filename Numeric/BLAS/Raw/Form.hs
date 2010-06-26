{-# LANGUAGE MagicHash, MultiParamTypeClasses, TypeFamilies #-}
module Numeric.BLAS.Raw.Form
    ( Pure
    , Raw(..)
    , Form(..)
    , Input(..)
    , Output(..)
    ) where

import Control.Monad.ST
import Data.Primitive.ByteArray
import Data.Primitive.Types

data Pure a 

class Form t v a where
    data family Raw t (v :: * -> *) a
    size :: Raw t v a -> Int
    aliases :: Raw t v a -> Raw t v a -> Bool

class Form t v a => Input t v s a
    read         :: Raw t v a -> Int -> ST s a
    unsafeFreeze :: Raw t v a -> ST s (Raw t Pure a)
    thaw         :: Output t (ST s) s a => Raw t v a -> ST s (Raw t (ST s) a)
    freeze       :: Raw t v a -> ST s (Raw t Pure a)

    -- parallelize?
    thaw input = do
        output <- new s
        forM_ [0..s-1] $ \i -> do
            e <- read input i
            write output i e
      where 
        s = size a

    freeze a = do
        a' <- thaw a 
        unsafeFreeze a'

class (Input t Pure s a, Input t (ST s) s a) => Output t s a where
    new   :: Int -> ST s (Raw t (ST s) a)
    write :: Raw t (ST s) a -> Int -> a -> ST s ()
