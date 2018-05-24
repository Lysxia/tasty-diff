{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GenDiff where

import Control.Monad (replicateM)
import Data.Bifunctor (bimap)
import Data.String (fromString)

import Data.Algorithm.Diff
  ( Diff(..), getGroupedDiff )
import Test.QuickCheck

import Test.Tasty.Diff.Internal.Diff

pattern DiffList :: [a] -> [a] -> [Diff [a]]
pattern DiffList xs ys <- (extractDiff -> (xs, ys))

instance Arbitrary a => Arbitrary (Diff a) where
  arbitrary = oneof
    [ First  <$> scale sqrt' arbitrary  -- Edits don't need to be long
    , Second <$> scale sqrt' arbitrary
    , both <$> arbitrary
    ] where sqrt' = floor . sqrt . (fromIntegral :: Int -> Double)
  shrink (First as) = both as : fmap First (shrink as)
  shrink (Second as) = both as : fmap Second (shrink as)
  shrink (Both as _) = fmap both (shrink as)

extractDiff :: [Diff [t]] -> ([t], [t])
extractDiff = bimap concat concat . unzip . fmap fromDiff where
  fromDiff (Both a b) = (a, b)
  fromDiff (First  a) = (a, [])
  fromDiff (Second b) = ([], b)

-- Sample the number of groups of random diffs
splGps :: IO Int
splGps = do
  xs <- replicateM 1000 $ generate arbitrary >>= \(DiffList xs ys) ->
    let DiffGroups gs = contextDiff 3 (getGroupedDiff (xs :: [Int]) ys)
    in return (length gs)
  return (sum xs)

printRandomDiff :: IO ()
printRandomDiff = do
  DiffList xs ys <- generate arbitrary
  let f = fmap (fromString . show)
  printGroupedDiff 3 "This" "That" (getGroupedDiff (f xs) (f (ys :: [Int])))
