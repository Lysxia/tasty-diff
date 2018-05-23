import Data.Bifunctor (bimap)
import Data.Algorithm.Diff
  ( Diff(..), getGroupedDiff )

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Tasty.Diff.Internal.Diff

type X = Int

prop_getGroupedDiff :: [X] -> [X] -> Bool
prop_getGroupedDiff xs ys = goodGroupedDiff ds where
  ds = getGroupedDiff xs ys
  -- -- To check statistics
  -- label prop = label (mkLabel ds)
  -- mkLabel (d : ds') = (case d of Both{} -> 'B'; First{} -> 'F'; Second{} -> 'S') : mkLabel ds'
  -- mkLabel [] = []

prop_contextDiff :: CtxLen_ -> [X] -> [X] -> Bool
prop_contextDiff (CtxLen_ ctxLen) xs ys = goodContextDiff gs where
  ds = getGroupedDiff xs ys
  gs = contextDiff ctxLen ds

prop_correct_getGroupedDiff :: [X] -> [X] -> Property
prop_correct_getGroupedDiff xs ys = correctDiff xs ys ds where
  ds = getGroupedDiff xs ys

prop_correct_contextDiff :: CtxLen_ -> [X] -> [X] -> Property
prop_correct_contextDiff (CtxLen_ ctxLen) xs ys = correctDiff xs ys gs0 where
  ds = getGroupedDiff xs ys
  DiffGroups gs_ = contextDiff ctxLen ds
  gs0 = concat gs_

main :: IO ()
main = defaultMain $ testGroup "Test"
  [ testProperty "correct_getGroupedDiff" prop_correct_getGroupedDiff
  , testProperty "getGroupedDiff" prop_getGroupedDiff
  , testProperty "correct_contextDiff" prop_correct_contextDiff
  , testProperty "contextDiff" prop_contextDiff
  ]

goodGroupedDiff :: Eq t => [Diff [t]] -> Bool
goodGroupedDiff ds = case ds of
  [] -> True
  Both _ _ : Both _ _ : _ -> False
  First _ : First _ : _ -> False
  Second _ : Second _ : _ -> False
  Second _ : First _ : _ -> False
  First [] : _ -> False
  Second [] : _ -> False
  Both [] _ : _ -> False
  Both xs' ys' : _ | xs' /= ys' -> False
  _ : ds' -> goodGroupedDiff ds'

newtype CtxLen_ = CtxLen_ CtxLen
  deriving Show

instance Arbitrary CtxLen_ where
  arbitrary = CtxLen_ <$> choose (0, 10)
  shrink (CtxLen_ x) = CtxLen_ <$> shrink x

goodContextDiff :: Eq t => DiffGroups t -> Bool
goodContextDiff (DiffGroups gs) = goodContextDiff' gs

goodContextDiff' :: Eq t => [[Diff [t]]] -> Bool
goodContextDiff' gs = case gs of
  [] -> True
  [] : _ -> False  -- No empty diff group
  [Both _ _] : [Both _ _] : _ -> False  -- No consecutive trivial diffs
  (Both _ _ : g@(_ : _)) : _ | all isBoth g -> False  -- Trivial diffs have length 1
  g : gs' -> goodGroupedDiff g && goodContextDiff' gs'

extractDiff :: [Diff [t]] -> ([t], [t])
extractDiff = bimap concat concat . unzip . fmap fromDiff where
  fromDiff (Both a b) = (a, b)
  fromDiff (First  a) = (a, [])
  fromDiff (Second b) = ([], b)

correctDiff :: (Eq t, Show t) => [t] -> [t] -> [Diff [t]] -> Property
correctDiff xs ys ds = extractDiff ds === (xs, ys)