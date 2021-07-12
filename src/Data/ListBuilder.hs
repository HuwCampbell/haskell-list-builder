{-# LANGUAGE DoAndIfThenElse, BangPatterns #-}
-- | Mutable List Builder.
--
--   A `ListBuilder s a` is a wrapper over `ST s [a]`, but uses unsafe
--   mutation to achieve constant time append as well as prepend.
--
--   As the builder is backed with a standard 'Data.List.List', it
--   is light-weight and cheap to return to a list.
--
--   Code from this module is derived from Scala's ListBuffer module,
--   using the unsafe set field technique described by
--   [Twan van Laarhoven](https://www.twanvl.nl/blog/haskell/unsafe-sequence).
module Data.ListBuilder (
    ListBuilder

  , newBuilder
  , append
  , prepend
  , length
  , freeze
  , unsafeFreeze
  ) where

import Data.ListBuilder.Unsafe

import Control.Monad (when)
import Control.Monad.ST
import Control.Monad.Primitive

import Control.Monad.ST.Unsafe

import Data.Foldable (foldr')
import Data.STRef
    ( STRef,
      modifySTRef,
      modifySTRef',
      newSTRef,
      readSTRef,
      writeSTRef )

import Prelude hiding (length)

-- | A List Builder.
--
--   This builder is backed by a standard haskell 'Data.List.List'.
--   It offers predictable (and fast) operations, and doesn't
--   pause for grow operations as an array based builder might.
data ListBuilder s a = ListBuilder {
    start :: STRef s [a]
  , end :: STRef s [a]
  , len :: STRef s Int
}

-- | Create a new, empty 'ListBuilder'
newBuilder :: PrimMonad m => m (ListBuilder (PrimState m) a)
newBuilder = stToPrim $ do
  start <- newSTRef []
  end   <- newSTRef []
  len   <- newSTRef 0
  pure $
    ListBuilder start end len

-- | Append an item to the back of the 'ListBuilder'
--
--   /O(1)/
append :: PrimMonad m => a -> ListBuilder (PrimState m) a -> m ()
append a bldr = stToPrim $ do
  let
    !last' = [a]
  len' <- readSTRef (len bldr)

  if len' == 0 then do
    writeSTRef (start bldr) last'
    writeSTRef (end bldr) last'
  else do
    end' <- readSTRef (end bldr)
    unsafeIOToST $
      unsafeSetField 1 end' last'
    writeSTRef (end bldr) last'

  modifySTRef' (len bldr) (+1)

-- | Prepend an item to the front of the 'ListBuilder'
--
--   /O(1)/
prepend :: PrimMonad m => a -> ListBuilder (PrimState m) a -> m ()
prepend a bldr = stToPrim $ do
  front <- readSTRef (start bldr)
  len'  <- readSTRef (len bldr)

  let
    !front' = a : front

  when (len' == 0) $
    writeSTRef (end bldr) front'

  writeSTRef (start bldr) front'
  modifySTRef (len bldr) (+1)


-- | The current length of the 'ListBuilder'.
--
--   /O(1)/
length :: PrimMonad m => ListBuilder (PrimState m) a -> m Int
length bldr = stToPrim $
  readSTRef (len bldr)





-- | Return the 'Data.List.List' backing the 'ListBuilder'.
--
--   This does _not_ stop mutations made to
--   the builder from affecting the resultant
--   list. So one must not continue to call the
--   mutating functions.
--
--   /O(1)/
unsafeFreeze :: PrimMonad m => ListBuilder (PrimState m) a -> m [a]
unsafeFreeze bldr = stToPrim $
  readSTRef (start bldr)


-- | Freeze the result and return the list.
--
--   This function strictly copies the spine
--   of the list.
--
--   /O(n)/
freeze :: PrimMonad m => ListBuilder (PrimState m) a -> m [a]
freeze bldr = do
  aliased <- stToPrim $
    readSTRef (start bldr)

  return $!
    foldr' (:) [] aliased
