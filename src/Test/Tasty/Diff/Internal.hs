{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.Diff.Internal where

import Control.Exception (catchJust, finally)
import Control.Monad (guard)
import Data.IORef
import Data.Proxy (Proxy(..))
import System.IO (Handle, IOMode(..), withBinaryFile)
import System.IO.Error (isDoesNotExistError)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Test.Tasty.Ingredients.ConsoleReporter
  ( Quiet(..), useColor )
import Test.Tasty.Options
  ( IsOption(..), OptionDescription(..), OptionSet
  , flagCLParser, lookupOption, safeReadBool )
import Test.Tasty.Providers
  ( IsTest(..), TestName, TestTree, singleTest, testFailed, testPassed )

import Test.Tasty.Diff.Internal.Diff

-- | The golden test core configuration.
data GoldenTest = GoldenTest
  { referencePath :: GoldenPath
  , getReference :: IO (Maybe Ref)
  , setReference :: Out -> IO ()
  , getOutput :: IO Out
  , runDiff :: Ref -> Out -> TermIO Cmp
  }

type Lines = [Text]
type Ref = Lines
type Out = Lines

-- | Comparison result, with an error message in case it fails.
data Cmp = NotEqual String | Equal

instance IsTest GoldenTest where
  run options GoldenTest{..} _ = do
    out  <- getOutput
    ref_ <- getReference
    case ref_ of
      Nothing -> do
        setReference out
        return (testPassed $ "Golden file created (" ++ referencePath ++ ")")
      Just ref -> do
        cmp <- runTerm (sneak options) (runDiff ref out)
        case cmp of
          Equal -> return $ testPassed ""
          NotEqual diff
            | Accept <- lookupOption options -> do
              setReference out
              return (testPassed $ "Golden file overwritten (" ++ referencePath ++ ")")
            | otherwise ->
              return (testFailed diff)
  testOptions = pure
    [ Option (Proxy :: Proxy Accept)
    ]

sneak :: OptionSet -> SneakyOptions
sneak options = SneakyOptions
  { quiet = quiet
  , colors = colors }
  where
    o :: IsOption o => o
    o = lookupOption options
    quiet | NoAccept <- o, Quiet False <- o = False
          | otherwise = True
    colors = useColor o

type GoldenPath = FilePath
type OutPath = FilePath

-- | 'GoldenTest' constructor to consistently populate the first three fields.
mkGoldenTest
  :: ([Text] -> [Text] -> TermIO Cmp)  -- ^ Diff function
  -> GoldenPath  -- ^ Golden file
  -> IO [Text]   -- ^ Test output
  -> GoldenTest
mkGoldenTest diff refpath getOut = GoldenTest
  { referencePath = refpath
  , getReference = mkGetReference refpath
  , setReference = mkSetReference refpath
  , getOutput = getOut
  , runDiff = diff
  }

-- | Compare test output (as a list of lines) with golden file.
goldenTestLinesWith
  :: DiffOption  -- ^ Diff variant
  -> TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> IO [Text]   -- ^ Test output (lines)
  -> TestTree
goldenTestLinesWith diffOpt tname refpath getOut =
  singleTest tname (mkGoldenTest diff refpath getOut)
  where
    diff = case diffOpt of
      EqNoDiff -> runEqNoDiff refpath
      EqDiff -> runEqDiff refpath
      SimpleDiff ctxLen -> runSimpleDiff ctxLen refpath tname
      PrettyDiff ctxLen hook -> runPrettyDiff hook ctxLen refpath tname

-- | Compare test output with golden file.
goldenTestWith
  :: DiffOption  -- ^ Diff variant
  -> TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> IO Text     -- ^ Test output
  -> TestTree
goldenTestWith diffOpt tname refpath getOut =
  goldenTestLinesWith diffOpt tname refpath (fmap Text.lines getOut)

-- | Compare test output with golden file.
--
-- The test should write its output to the given handle.
goldenTestFileWith
  :: DiffOption  -- ^ Diff variant
  -> TestName    -- ^ Test name
  -> GoldenPath  -- ^ Golden file
  -> OutPath     -- ^ Test output file
  -> (Handle -> IO ())  -- ^ Test program (writes to handle)
  -> TestTree
goldenTestFileWith diffOpt tname refPath outPath writeOut =
  goldenTestWith diffOpt tname refPath getOut
  where
    getOut = do
      withBinaryFile outPath WriteMode writeOut
      readBinaryFile outPath

-- | Write 'Text' to file as binary (no newline conversion).
writeBinaryFile :: FilePath -> Text -> IO ()
writeBinaryFile path txt =
  withBinaryFile path WriteMode $ \h ->
    Text.hPutStr h txt

-- | Read 'Text' from file as binary (no newline conversion).
readBinaryFile :: FilePath -> IO Text
readBinaryFile path =
  withBinaryFile path ReadMode Text.hGetContents

mkGetReference :: GoldenPath -> IO (Maybe Ref)
mkGetReference refpath =
  catchJust
    (guard . isDoesNotExistError)
    (fmap (Just . Text.lines) (readBinaryFile refpath))
    (\() -> return Nothing)

mkSetReference :: GoldenPath -> Ref -> IO ()
mkSetReference refpath r =
  writeBinaryFile refpath (Text.unlines r)

-- | Variants of text comparison implemented by this library.
data DiffOption
  = EqNoDiff
  -- ^ Use 'Eq', no diff output.

  | EqDiff
  -- ^ Use 'Eq', print the two compared strings if they differ.

  | SimpleDiff CtxLen
  -- ^ Print a diff of the two strings.
  --
  -- 'CtxLen' is the number of lines of context surrounding each diff block.
  --
  -- See also 'simpleDiff'.

  | PrettyDiff CtxLen Hook
  -- ^ Print a pretty diff of the two strings (prettier than 'SimpleDiff').
  -- The diff will be printed after all tests are finished, so as not
  -- to mangle @tasty@'s terminal output.
  --
  -- 'CtxLen' is the number of lines of context surrounding each diff block.
  -- 'Hook' is an abstract type generated by the 'mainWithHook' bracket.
  --
  -- See also 'prettyDiff'.

-- | Print a diff of the two strings.
--
-- Default 'CtxLen' value of 3 ('defaultCtxLen'). See also 'SimpleDiff'.
simpleDiff :: DiffOption
simpleDiff = SimpleDiff defaultCtxLen

-- | Print a colored diff of the two strings.
--
-- Default 'CtxLen' value of 3 ('defaultCtxLen'). See also 'PrettyDiff'.
prettyDiff :: Hook -> DiffOption
prettyDiff = PrettyDiff defaultCtxLen

-- | A simple 'Cmp' wrapper around @('==')@.
runEqNoDiff :: (Applicative f, Eq a) => GoldenPath -> a -> a -> f Cmp
runEqNoDiff refpath a b = pure $
  if a == b then
    Equal
  else
    NotEqual $ "Output differs from golden file (" ++ refpath ++ ")"

runEqDiff :: Applicative f => GoldenPath -> [Text] -> [Text] -> f Cmp
runEqDiff refpath a b = pure $
  if a == b then
    Equal
  else
    NotEqual . unlines $
      [ "Output differs from golden file (" ++ refpath ++ ")"
      , "Expected: "
      ] ++ indent a ++
      [ "Actual: "
      ] ++ indent b
  where
    indent = fmap (("  " ++) . Text.unpack)

mkRunDiff
  :: Applicative f
  => (GroupedDiff Text -> f String)
  -> Ref -> Out -> f Cmp
mkRunDiff render ref out =
  if isTrivialDiff diff then
    pure Equal
  else
    fmap NotEqual (render diff)
  where
    diff = getGroupedDiff ref out

type DiffRunner
  = CtxLen -> GoldenPath -> TestName -> Ref -> Out -> TermIO Cmp

runSimpleDiff :: DiffRunner
runSimpleDiff ctxLen refpath tname =
  mkRunDiff (return . renderGroupedDiffString ctxLen refpath tname)

-- | An abstract type generated by the 'mainWithHook' bracket.
newtype Hook = Hook (IORef (IO ()))

runPrettyDiff :: Hook -> DiffRunner
runPrettyDiff (Hook r0) ctxLen refpath tname =
  mkRunDiff (\diff -> termIO $ \opts -> do
    atomicModifyIORef' r0 $ \render ->
      let run' = render *> runTerm opts (renderGroupedDiff ctxLen refpath tname diff)
      in (run', ())
    return $ "Output differs from golden file " ++ refpath)

-- | Bracket function allowing colored diffs to be printed with after all tests
-- have run. This is to avoid mangling @tasty@'s terminal output.
--
-- Provides a 'Hook' for 'Test.Tasty.Diff.goldenTest'.
--
-- > main :: IO ()
-- > main = mainWithHook $ \hook -> defaultMain $ testGroup "test"
-- >   [ Pretty.goldenTest hook "TestExample"
-- >       "example/golden.txt"
-- >       getOutput
-- >   ]
-- >
-- > getOutput :: IO Text
-- > getOutput = return "42"
mainWithHook :: (Hook -> IO ()) -> IO ()
mainWithHook k = do
  r0 <- newIORef (return ())
  finally (k (Hook r0)) $ do
    render <- readIORef r0
    render

data Accept = NoAccept | Accept

instance IsOption Accept where
  defaultValue = NoAccept
  parseValue = (fmap . fmap) f safeReadBool
    where
      f True = Accept
      f False = NoAccept
  optionName = pure "accept"
  optionHelp = pure "Accept current results of golden tests"
  optionCLParser = flagCLParser Nothing Accept
