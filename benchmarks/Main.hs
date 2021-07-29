{-# LANGUAGE OverloadedStrings #-}
module Main where

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
          bench "remake"    $ whnfIO (remake small)
        , bench "reverse"   $ whnfIO (rev small)
        , bench "cons-snoc" $ whnfIO (consSnoc small)
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
          bench "remake"  $ whnfIO (remake large)
        , bench "reverse" $ whnfIO (rev large)
        , bench "cons-snoc" $ whnfIO (consSnoc large)
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

remake :: [a] -> IO [a]
remake xs = do
  bldr <- ListBuilder.newBuilder
  for_ xs $ \x ->
    ListBuilder.append x bldr
  ListBuilder.unsafeFreeze bldr


rev :: [a] -> IO [a]
rev xs = do
  bldr <- ListBuilder.newBuilder
  for_ xs $ \x ->
    ListBuilder.prepend x bldr
  ListBuilder.unsafeFreeze bldr


consSnoc :: [a] -> IO [a]
consSnoc xs = do
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
