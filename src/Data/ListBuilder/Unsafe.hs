{-# LANGUAGE MagicHash, UnboxedTuples, GHCForeignImportPrim, UnliftedFFITypes, BangPatterns #-}
-- | This module allows for the (obviously unsafe) overwriting of one haskell value with another.
module Data.ListBuilder.Unsafe (
     unsafeSetField
  ,  unsafeGetField
  ) where

import GHC.IO
import GHC.Prim
import GHC.Exts

foreign import prim "unsafeSetFieldzh" unsafeSetField# :: Int# -> Any -> Any -> (##)
foreign import prim "unsafeGetFieldzh" unsafeGetField# :: Int# -> Any -> (# Any #)

-- Set the value of a certain constructor field.
-- You'd better be careful that it isn't being shared
unsafeSetField :: Int -> a -> b -> IO ()
unsafeSetField (I# i) !x y = case unsafeSetField# i (unsafeCoerce# x :: Any) (unsafeCoerce# y :: Any) of
  (##) -> return ()
{-# INLINE unsafeSetField #-}

unsafeGetField :: Int -> a -> IO b
unsafeGetField (I# i) !x = case unsafeGetField# i (unsafeCoerce# x :: Any) of
  (# y #) -> return (unsafeCoerce# y)
{-# INLINE unsafeGetField #-}
