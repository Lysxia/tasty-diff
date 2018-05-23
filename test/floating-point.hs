import Data.String (fromString)
import Data.Text (Text)

import Test.Tasty

import Test.Tasty.Diff

main :: IO ()
main = mainWithHook $ \hook -> defaultMain $ testGroup "Test"
  [ goldenTestLines hook "floor"   "golden/floating-point.golden" (test floor)
  , goldenTestLines hook "ceiling" "golden/floating-point.golden" (test ceiling)
  ]

test :: (Double -> Integer) -> IO [Text]
test f = pure $
  fmap (fromString . show)
    [f (n * 0.01 * 105) | n <- [30 .. 60]]
