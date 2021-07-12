Haskell List Builder
====================

Extra unsafe sequencing of IO actions from [Twan van Laarhoven](https://www.twanvl.nl/blog/haskell/unsafe-sequence)
packaged up, along with `unfoldIO` and an implementation of Scala's List Buffer. This means we can have tail
recursive `sequence` and `traverse` specialised for IO lists.

For algorithms which are more easily written in a mutable way we have `ListBuilder s a`. A `ListBuilder s a`
is a wrapper around an `ST s [a]`, but with a constant time append using unsafe tricks.
