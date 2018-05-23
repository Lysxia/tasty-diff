module Test.Tasty.Diff.Simple
  ( -- * Main combinators
    goldenTest
  , goldenTestLines
  , goldenTestFile

    -- * Types
  , GoldenPath
  , OutPath
  , CtxLen

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
  :: TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> IO Text     -- ^ Test output
  -> TestTree
goldenTest = goldenTestWith simpleDiff

-- | Compare test output (as a list of lines) with golden file.
goldenTestLines
  :: TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> IO [Text]   -- ^ Test output (lines)
  -> TestTree
goldenTestLines = goldenTestLinesWith simpleDiff

-- | Compare test output with golden file.
--
-- The test should write its output to the given handle.
goldenTestFile
  :: TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> OutPath     -- ^ Test output file
  -> (Handle -> IO ())  -- ^ Test program (writes to handle)
  -> TestTree
goldenTestFile = goldenTestFileWith simpleDiff

-- With CtxLen parameter

-- | Compare test output with golden file.
goldenTestCtx
  :: CtxLen      -- ^ Number of context lines
  -> TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> IO Text     -- ^ Test output
  -> TestTree
goldenTestCtx = goldenTestWith . SimpleDiff

-- | Compare test output (as a list of lines) with golden file.
goldenTestLinesCtx
  :: CtxLen      -- ^ Number of context lines
  -> TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> IO [Text]   -- ^ Test output (lines)
  -> TestTree
goldenTestLinesCtx = goldenTestLinesWith . SimpleDiff

-- | Compare test output with golden file.
--
-- The test should write its output to the given handle.
goldenTestFileCtx
  :: CtxLen      -- ^ Number of context lines
  -> TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> OutPath     -- ^ Test output file
  -> (Handle -> IO ())  -- ^ Test program (writes to handle)
  -> TestTree
goldenTestFileCtx = goldenTestFileWith . SimpleDiff
