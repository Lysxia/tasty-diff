{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Providers (singleTest)
import Test.Tasty.GoldenText.Internal
import Test.Tasty.GoldenText.Internal.Diff (defaultCtxLen)

main :: IO ()
main = mainWithHook $ \hook -> defaultMain $ testGroup "Test"
  [ singleTest "Good" dummyTest
  , singleTest "Bad" dummyTest'
  , singleTest "Pretty" (prettyTest hook)
  ]

dummyTest :: GoldenTest
dummyTest = GoldenTest
  { getReference = return (Just ["12"])
  , setReference = \_ -> return ()
  , getOutput = return ["12"]
  , runDiff = runSimpleDiff defaultCtxLen "Jim" "Joe"
  }

dummyTest' :: GoldenTest
dummyTest' = dummyTest { getOutput = return ["13"] }

prettyTest :: Hook -> GoldenTest
prettyTest hook = dummyTest'
  { runDiff = runPrettyDiff hook defaultCtxLen "X.txt" "Joe" }
