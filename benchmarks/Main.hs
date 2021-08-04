{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.ST
import Criterion
import Criterion.Main
import Data.Foldable
import qualified Data.DList as DList
import Data.ListBuilder as ListBuilder

main :: IO ()
main = do
  let
    small = replicate 1000 'A'
    large = replicate 1000000 'A'

  defaultMain [
      bgroup "small" [
        bgroup "list-builder" [
          bench "remake"    $ whnf remake small
        , bench "reverse"   $ whnf rev small
        , bench "cons-snoc" $ whnf consSnoc small
        ]
      , bgroup "pure-list" [
          bench "remake"    $ nf (foldr' (:) []) (small)
        , bench "reverse"   $ nf (reverse) (small)
        , bench "cons-snoc" $ nf (foldl' (flip pureConsSnoc) []) (small)
        ]
      , bgroup "diff-list" [
          bench "remake"    $ nf (DList.toList . foldl' (flip DList.cons) DList.empty) small
        , bench "reverse"   $ nf (DList.toList . foldl' (DList.snoc) DList.empty) small
        , bench "cons-snoc" $ nf (DList.toList . foldl' (flip diffConsSnoc) DList.empty) small
        ]
      ]

    , bgroup "large" [
        bgroup "list-builder" [
          bench "remake"  $ nf remake large
        , bench "reverse" $ nf rev large
        , bench "cons-snoc" $ nf consSnoc large
        ]
      , bgroup "pure-list" [
          bench "remake"  $ nf (foldr (:) []) large
        , bench "reverse" $ nf reverse large
        ]
      , bgroup "diff-list" [
          bench "remake"  $ nf (DList.toList . foldl' (flip DList.cons) DList.empty) large
        , bench "reverse" $ nf (DList.toList . foldl' (DList.snoc) DList.empty) large
        , bench "cons-snoc" $ nf (DList.toList . foldl' (flip diffConsSnoc) DList.empty) large
        ]
      ]
    ]

remake :: [a] -> [a]
remake xs = runST $ do
  bldr <- ListBuilder.newBuilder
  for_ xs $ \x ->
    ListBuilder.append x bldr
  ListBuilder.unsafeFreeze bldr


rev :: [a] -> [a]
rev xs = runST $ do
  bldr <- ListBuilder.newBuilder
  for_ xs $ \x ->
    ListBuilder.prepend x bldr
  ListBuilder.unsafeFreeze bldr


consSnoc :: [a] -> [a]
consSnoc xs = runST $ do
  bldr <- ListBuilder.newBuilder
  for_ xs $ \x -> do
    ListBuilder.append x bldr
    ListBuilder.prepend x bldr
  ListBuilder.unsafeFreeze bldr

diffConsSnoc :: a -> DList.DList a -> DList.DList a
diffConsSnoc x dxs =
  DList.cons x (DList.snoc dxs x)

pureConsSnoc :: a -> [a] -> [a]
pureConsSnoc x xs =
  x : xs <> [x]
