module Test.Tasty.Diff
  ( -- * Main combinators
    goldenTest
  , goldenTestLines
  , goldenTestFile
  , mainWithHook

    -- * Types
  , GoldenPath
  , OutPath
  , CtxLen
  , Hook

    -- * Utilities
  , readBinaryFile
  , writeBinaryFile

    -- * With 'CtxLen' parameter
  , goldenTestCtx
  , goldenTestLinesCtx
  , goldenTestFileCtx
  ) where

import Data.Text (Text)
import System.IO (Handle)

import Test.Tasty
  ( TestName, TestTree )

import Test.Tasty.Diff.Internal
import Test.Tasty.Diff.Internal.Diff

-- Main combinators

-- | Compare test output with golden file.
goldenTest
  :: Hook
  -> TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> IO Text     -- ^ Test output
  -> TestTree
goldenTest = goldenTestWith . prettyDiff

-- | Compare test output (as a list of lines) with golden file.
goldenTestLines
  :: Hook
  -> TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> IO [Text]   -- ^ Test output (lines)
  -> TestTree
goldenTestLines = goldenTestLinesWith . prettyDiff

-- | Compare test output with golden file.
--
-- The test should write its output to the given handle.
goldenTestFile
  :: Hook
  -> TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> OutPath     -- ^ Test output file
  -> (Handle -> IO ())  -- ^ Test program (writes to handle)
  -> TestTree
goldenTestFile = goldenTestFileWith . prettyDiff

-- With CtxLen parameter

-- | Compare test output with golden file.
goldenTestCtx
  :: CtxLen      -- ^ Number of context lines
  -> Hook
  -> TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> IO Text     -- ^ Test output
  -> TestTree
goldenTestCtx = goldenTestWith .: PrettyDiff

(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) = fmap fmap fmap

-- | Compare test output (as a list of lines) with golden file.
goldenTestLinesCtx
  :: CtxLen      -- ^ Number of context lines
  -> Hook
  -> TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> IO [Text]   -- ^ Test output (lines)
  -> TestTree
goldenTestLinesCtx = goldenTestLinesWith .: PrettyDiff

-- | Compare test output with golden file.
--
-- The test should write its output to the given handle.
goldenTestFileCtx
  :: CtxLen      -- ^ Number of context lines
  -> Hook
  -> TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> OutPath     -- ^ Test output file
  -> (Handle -> IO ())  -- ^ Test program (writes to handle)
  -> TestTree
goldenTestFileCtx = goldenTestFileWith .: PrettyDiff
