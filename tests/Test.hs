{-# LANGUAGE TemplateHaskell #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Main as Main

import           Control.Monad.ST
import           Data.Foldable
import           Data.IORef

import qualified Data.ListBuilder as ListBuilder
import           Data.Traversable.IO

prop_cons :: Property
prop_cons =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    let
      result = runST $  do
        bldr <- ListBuilder.newBuilder
        for_ xs $ \x ->
          ListBuilder.append x bldr
        ListBuilder.freeze bldr
    result === xs

prop_reverses :: Property
prop_reverses =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    let
      result = runST $ do
        bldr <- ListBuilder.newBuilder
        for_ xs $ \x ->
          ListBuilder.prepend x bldr
        ListBuilder.freeze bldr
    reverse result === xs

prop_sequenceIO :: Property
prop_sequenceIO =
  property $ do
    refs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    xs   <- evalIO $ sequenceIO (fmap return refs)
    xs === refs

prop_unfoldIO :: Property
prop_unfoldIO =
  property $ do
    refs  <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha

    items <- evalIO $ newIORef refs
    xs    <- evalIO $ unfoldIO $ do
      remaining <- readIORef items
      case remaining of
        [] -> return Nothing
        n : ns -> do
          writeIORef items ns
          return (Just n)

    xs === refs


prop_freeze_is_safe :: Property
prop_freeze_is_safe =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    y  <- forAll Gen.alpha
    z  <- forAll Gen.alpha
    let
      (result, withTips) = runST $  do
        bldr <- ListBuilder.newBuilder
        for_ xs $ \x ->
          ListBuilder.append x bldr

        built <-
          ListBuilder.freeze bldr

        ListBuilder.append y bldr
        ListBuilder.prepend z bldr

        withTips' <-
          ListBuilder.freeze bldr

        return (built, withTips')

    result   === xs
    withTips === (z : xs <> [y])


prop_filter_in_place :: Property
prop_filter_in_place =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 10))
    let
      result = runST $  do
        bldr <- ListBuilder.newBuilder
        for_ xs $ \x ->
          ListBuilder.append x bldr

        ListBuilder.filterInPlace even bldr
        ListBuilder.freeze bldr

    result === filter even xs


prop_insert_satisfies :: Property
prop_insert_satisfies = do
  property $ do
    let gInt = Gen.int (Range.linear 0 10)
    xs <- forAll $ Gen.list (Range.linear 0 100) gInt
    aa <- forAll gInt
    ys <- forAll $ Gen.list (Range.linear 0 100) gInt
    let
      result = runST $ do
        bldr <- ListBuilder.newBuilder
        for_ xs $ \x ->
          ListBuilder.append x bldr
        for_ ys $ \x ->
          ListBuilder.append x bldr

        ListBuilder.insert (length xs) aa bldr

        ListBuilder.unsafeFreeze bldr

    result   === xs <> [aa] <> ys

tests :: IO Bool
tests =
  checkParallel $$(discover)

main :: IO ()
main =
  Main.defaultMain [
    tests
  ]
