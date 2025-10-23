{-# LANGUAGE DoAndIfThenElse, BangPatterns, NamedFieldPuns #-}
-- | Mutable List Builder.
--
--   A @ListBuilder s a@ is a wrapper over @ST s [a]@, but uses unsafe
--   mutation to achieve constant time append as well as prepend.
--
--   As the builder is backed with a standard 'Data.List.List', it
--   is light-weight and cheap to return to a list.
--
--   Code from this module is derived from Scala's
--   [ListBuffer](https://www.scala-lang.org/api/current/scala/collection/mutable/ListBuffer.html)
--   module, using the unsafe set field technique described by
--   [Twan van Laarhoven](https://www.twanvl.nl/blog/haskell/unsafe-sequence).
module Data.ListBuilder (
    -- * Mutable list builder
    ListBuilder

  -- * Construction
  , newBuilder

  -- * Mutations
  , append
  , prepend
  , insert
  , filterInPlace
  , clear

  -- * Accessors
  , readLength
  , readFirst
  , readLast
  , readAt

  -- * Conversions
  , freeze
  , unsafeFreeze
  ) where

import Data.ListBuilder.Unsafe
import qualified Data.List

import Control.Monad (when)
import Control.Monad.ST
import Control.Monad.ST.Unsafe

import Data.Foldable (foldr')
import Data.Maybe (listToMaybe)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')

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
newBuilder :: ST s (ListBuilder s a)
newBuilder = do
  start <- newSTRef []
  end   <- newSTRef []
  len   <- newSTRef 0
  pure $
    ListBuilder start end len

-- | Append an item to the back of the 'ListBuilder'
--
--   /O(1)/
append :: a -> ListBuilder s a -> ST s ()
append a ListBuilder { start, end, len } = do
  let
    !last' = [a]
  len' <- readSTRef len

  if len' == 0 then do
    writeSTRef start last'
    writeSTRef end last'
  else do
    end' <- readSTRef end
    unsafeIOToST $
      unsafeSetField 1 end' last'
    writeSTRef end last'

  modifySTRef' len (+1)

-- | Prepend an item to the front of the 'ListBuilder'
--
--   /O(1)/
prepend :: a -> ListBuilder s a -> ST s ()
prepend a ListBuilder { start, end, len } = do
  front <- readSTRef start
  len'  <- readSTRef len

  let
    !front' = a : front

  when (len' == 0) $
    writeSTRef end front'

  writeSTRef start front'
  modifySTRef' len (+1)


-- | Internal function. Locates the previous cons cell.
--
--   /O(N)/
locate :: Int -> ListBuilder s a -> ST s [a]
locate 0 _ = error "Internal error: locate called on 0"
locate i ListBuilder { start, end, len } = do
  l <- readSTRef len

  if l == i then
    readSTRef end

  else do
    start' <- readSTRef start
    cur    <- newSTRef start'
    let
      go 0 = readSTRef cur
      go j = do
        modifySTRef' cur (drop 1)
        go (j - 1)
    go (i - 1)

-- | Insert into a location in a 'ListBuilder'.
--
--   This function doesn't create a new spine
--   across the list builder, and only allocates
--   the new cons cell itself.
--
--   /O(N)/
insert :: Int -> a -> ListBuilder s a -> ST s ()
insert ix _ _ | ix < 0 = error "Index out of bounds"
insert 0 a bldr = prepend a bldr
insert ix a bldr= do
  len' <- readSTRef (len bldr)
  if ix == len' then
    append a bldr
  else if ix > len' then
    error "Index out of bounds"
  else do
    prev  <- locate ix bldr
    let !pn = drop 1 prev
    let !nx = a:pn
    unsafeIOToST $
      unsafeSetField 1 prev nx


-- | The current length of the 'ListBuilder'.
--
--   /O(1)/
readLength :: ListBuilder s a -> ST s Int
readLength bldr =
  readSTRef (len bldr)


-- | Empty the 'ListBuilder' of all values.
--
--   /O(1)/
clear :: ListBuilder s a -> ST s ()
clear ListBuilder { start, end, len } = do
  writeSTRef start []
  writeSTRef end []
  writeSTRef len 0


-- | Filter the 'ListBuilder' with the supplied predicate
--
--   /O(N)/
filterInPlace :: (a -> Bool) -> ListBuilder s a -> ST s ()
filterInPlace func ListBuilder { start, end, len } = do
  prev   <- newSTRef Nothing
  start' <- readSTRef start
  cur    <- newSTRef start'
  let
    go = do
      cur' <- readSTRef cur
      case cur' of
        [] -> return ()

        (h:follow) -> do
          prev' <- readSTRef prev
          if not (func h) then do
            case prev' of
              Nothing ->
                writeSTRef start follow
              Just y ->
                unsafeIOToST $
                  unsafeSetField 1 y follow

            modifySTRef' len (\x -> x - 1)
          else
            writeSTRef prev (Just cur')

          writeSTRef cur follow
          go

  go

  prev' <- readSTRef prev
  case prev' of
    Nothing -> do
      writeSTRef end []
    Just y ->
      writeSTRef end y


-- | Return the current last element in the 'ListBuilder'
--
--   /O(1)/
readLast :: ListBuilder s a -> ST s (Maybe a)
readLast ListBuilder { end } = do
  listToMaybe <$> readSTRef end



-- | Return the current first element in the 'ListBuilder'
--
--   /O(1)/
readFirst :: ListBuilder s a -> ST s (Maybe a)
readFirst ListBuilder { start } = do
  listToMaybe <$> readSTRef start


-- | Return the current element at a particular index for
--   the 'ListBuilder'
--
--   /O(N)/
readAt :: ListBuilder s a -> Int -> ST s (Maybe a)
readAt ListBuilder { start } ix = do
  (Data.List.!? ix) <$> readSTRef start


-- | Return the 'Data.List.List' backing the 'ListBuilder'.
--
--   This does /not/ stop mutations made to
--   the builder from affecting the resultant
--   list. So one must not continue to call the
--   mutating functions.
--
--   This function is safe in tail position within a
--   call to @runST@.
--
--   /O(1)/
unsafeFreeze :: ListBuilder s a -> ST s [a]
unsafeFreeze bldr =
  readSTRef (start bldr)


-- | Freeze the result and return the list.
--
--   This function strictly copies the spine
--   of the list.
--
--   /O(n)/
freeze :: ListBuilder s a -> ST s [a]
freeze bldr = do
  aliased <-
    readSTRef (start bldr)

  return $!
    foldr' (:) [] aliased
