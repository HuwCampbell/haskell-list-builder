-- | Traversing IO lists.
--
--   Code from this module is derived from [Twan van Laarhoven](https://www.twanvl.nl/blog/haskell/unsafe-sequence).
--
--   These operations are tail recursive, while building lists by appending
--   to its tail mutably.
module Data.Traversable.IO (
    sequenceIO
  , traverseIO
  , unfoldIO
) where

import Data.ListBuilder.Unsafe

import Prelude hiding (length)


-- | Traverse implemented in terms of unsafeSetField.
--
--   This operation is tail recursive.
traverseIO :: (a -> IO b) -> [a] -> IO [b]
traverseIO _ [] =
  return []
traverseIO f (mx0:xs0) = do
  x0 <- f mx0
  let front = [x0]
  go front xs0
  return front
    where
  go back [] = return ()
  go back (mx:xs) = do
    x <- f mx
    let back' = [x]
    unsafeSetField 1 back back'
    go back' xs
{-# INLINEABLE traverseIO #-}


-- | Sequence implemented in terms of unsafeSetField
--
--   This operation is tail recursive.
sequenceIO :: [IO a] -> IO [a]
sequenceIO =
  traverseIO id
{-# INLINE sequenceIO #-}


-- | Unfold a list from an IO action returning a 'Maybe' value.
--   As long as the function returns `Just`, its value will
--   be appended to the list.
unfoldIO :: IO (Maybe a) -> IO [a]
unfoldIO p = do
  x <- p
  case x of
    Nothing ->
      return []
    Just a -> do
      let front = [a]
      go front
      return front
    where
  go back = do
    x <- p
    case x of
      Nothing -> return ()
      Just a  -> do
        let back' = [a]
        unsafeSetField 1 back back'
        go back'
{-# INLINEABLE unfoldIO #-}
