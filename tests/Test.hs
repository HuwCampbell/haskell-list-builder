{-# LANGUAGE TemplateHaskell #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Main as Main

import           Control.Monad.ST
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.IORef

import qualified Data.ListBuilder as ListBuilder
import           Data.Traversable.IO

prop_cons :: Property
prop_cons =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 10000) Gen.alpha
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
    xs <- forAll $ Gen.list (Range.linear 0 10000) Gen.alpha
    result <- liftIO $  do
      bldr <- ListBuilder.newBuilder
      for_ xs $ \x ->
        ListBuilder.prepend x bldr
      ListBuilder.freeze bldr
    reverse result === xs

prop_sequenceIO :: Property
prop_sequenceIO =
  property $ do
    refs <- forAll $ Gen.list (Range.linear 0 1000) Gen.alpha
    xs   <- evalIO $ sequenceIO (fmap return refs)
    xs === refs

prop_unfoldIO :: Property
prop_unfoldIO =
  property $ do
    refs  <- forAll $ Gen.list (Range.linear 0 1000) Gen.alpha

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
    xs <- forAll $ Gen.list (Range.linear 0 10000) Gen.alpha
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

        withTips <-
          ListBuilder.freeze bldr

        return (built, withTips)

    result   === xs
    withTips === (z : xs <> [y])


tests :: IO Bool
tests =
  checkParallel $$(discover)

main :: IO ()
main =
  Main.defaultMain [
    tests
  ]
