Haskell List Builder
====================

Extra unsafe sequencing of IO actions from [Twan van Laarhoven](https://www.twanvl.nl/blog/haskell/unsafe-sequence)
packaged up, along with `unfoldIO` and an implementation of Scala's List Buffer. This means we can have tail
recursive `sequence` and `traverse` specialised for lists of IO actions.

For algorithms which are more easily written in a mutable way we have `ListBuilder s a`. A `ListBuilder s a`
is like a wrapper around an `ST s [a]`, but with a constant time append as well as prepend.

Basic usage:

```haskell
import qualified Data.ListBuilder as ListBuilder

palindrome :: [a] -> [a]
palindrome xs = runST $ do
  bldr <- ListBuilder.newBuilder
  for_ xs $ \x -> do
    ListBuilder.prepend x bldr
    ListBuilder.append x bldr
  ListBuilder.unsafeFreeze bldr
```


Other functions of interest:
```haskell
traverseIO :: (a -> IO b) -> [a] -> IO [b]
sequenceIO :: [IO a] -> IO [a]
unfoldIO :: IO (Maybe a) -> IO [a]
```
