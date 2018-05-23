{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.Diff.Internal.Diff
  ( module Test.Tasty.Diff.Internal.Diff
  , getGroupedDiff
  ) where

import Control.Monad (unless)
import Control.Applicative ((*>), Const(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Monoid (Endo(..), Sum(..))
import System.IO (stdout)

import Control.Monad.Trans.Reader

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.Algorithm.Diff
  ( Diff(..), getGroupedDiff )

import System.Console.ANSI
  ( SGR(..), ConsoleLayer(..), Color(..), ColorIntensity(..)
  , hSupportsANSI, setSGR )

-- * General purpose definitions

-- | Split @n@ elements from the end of the list.
--
-- > -- N.B.: if n < 0
-- > splitAtEnd n xs = (xs, [])
splitAtEnd :: Int -> [a] -> ([a], [a])
splitAtEnd n xs = splitAt (length xs - n) xs

-- ** Snoc lists

newtype SnocList a = SnocList [a]

fromSnocList :: SnocList a -> [a]
fromSnocList (SnocList xs) = reverse xs

snoc :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x = SnocList (x : xs)

-- * Diffs

-- | Simplified 'Both' constructor.
-- Invariant: the argument is nonempty (@t ~ [a]@)
both :: t -> Diff t
both x = Both x x

-- | The number of lines on each side contained in the diff.
diffLength :: Diff [t] -> (Sum Int, Sum Int)
diffLength (First  l) = (Sum (length l), 0)
diffLength (Second l) = (0, Sum (length l))
diffLength (Both l _) = (k, k) where k = Sum (length l)

-- | The number of lines on each side contained in the diff group.
diffGroupLength :: [Diff [t]] -> (Int, Int)
diffGroupLength = coerce . foldMap diffLength

-- | Flatten a diff group into a list of single-line diffs.
flattenDiff :: [Diff [t]] -> [Diff t]
flattenDiff gs = gs >>= \case
  First g -> fmap First g
  Second g -> fmap Second g
  Both g _ -> fmap both g

-- ** Grouping diffs with context

-- | Number of context lines surrounding each diff group.
type CtxLen = Int

-- | Default value: 3.
defaultCtxLen :: CtxLen
defaultCtxLen = 3

-- | Output type of 'getGroupedDiff', chunking together consecutive insertions
-- and consecutive deletions.
type GroupedDiff t = [Diff [t]]

-- | Grouped diffs with context, chunking together close sequences of
-- insertions and deletions.
newtype DiffGroups t = DiffGroups [[Diff [t]]]

emptyDiff :: DiffGroups t
emptyDiff = DiffGroups []

-- | Trivial diffs contain no insertions nor deletions, they are
-- lists of lines common to both files.
trivialDiff :: [t] -> DiffGroups t
trivialDiff [] = DiffGroups []
trivialDiff x = DiffGroups [[both x]]

addTrivialGroup :: [t] -> [Diff [t]] -> [Diff [t]]
addTrivialGroup [] = id
addTrivialGroup x = (both x :)

-- | Extend a diff with some context at the beginning.
consDiff :: [t] -> [Diff [t]] -> DiffGroups t -> DiffGroups t
consDiff x g (DiffGroups gs) = DiffGroups $ case x of
  [] -> g : gs
  _ -> [both x] : g : gs

isTrivialDiff :: [Diff t] -> Bool
isTrivialDiff = all isBoth

isBoth :: Diff t -> Bool
isBoth (Both _ _) = True
isBoth _ = False

-- | Chunk diffs with context.
contextDiff :: CtxLen -> [Diff [t]] -> DiffGroups t
contextDiff _      [] = emptyDiff
contextDiff _      (Both x _ : []) = trivialDiff x
contextDiff ctxLen (Both x _ : cs) =
  let (x0, x1) = splitAtEnd ctxLen x
      acc0 = SnocList (addTrivialGroup x1 [])
      (ds, cs') = splitDiff ctxLen acc0 cs
  in consDiff x0 ds (contextDiff ctxLen cs')
contextDiff ctxLen cs@(_ : _) =
  let (ds, cs') = splitDiff ctxLen (SnocList []) cs
  in consDiff [] ds (contextDiff ctxLen cs')

-- | Make one chunk, without the preceding context.
-- Returns the chunk and the remaining diff.
splitDiff
  :: CtxLen -> SnocList (Diff [t]) -> [Diff [t]] -> ([Diff [t]], [Diff [t]])
splitDiff _      acc [] = (fromSnocList acc, [])
splitDiff ctxLen acc (Both x _ : cs)
  | null cs || 2 * ctxLen + 1 < length x =
    let (x0, x1) = splitAt ctxLen x
        acc' = case x0 of
          [] -> acc
          _ : _ -> acc `snoc` both x0
    in (fromSnocList acc', addTrivialGroup x1 cs)
  | otherwise = splitDiff ctxLen (acc `snoc` both x) cs
-- c = (First _ | Second _)
splitDiff ctxLen acc (c : cs) = splitDiff ctxLen (acc `snoc` c) cs

-- ** Computing positions

-- | A diff group annotated with line numbers.
data LocatedDiff t = LocatedDiff
  { startL, endL, startR, endR :: Int
  , diffGroup :: [Diff [t]]
  }

locateDiffs :: DiffGroups t -> [LocatedDiff t]
locateDiffs (DiffGroups gs0) = go 0 0 gs0 where
  go _ _ [] = []
  go i j (g : gs) =
    let (hi, hj) = diffGroupLength g
        !i' = i + hi
        !j' = j + hj
        ldiff = LocatedDiff
          { startL = i, endL = i'
          , startR = j, endR = j'
          , diffGroup = g
          }
    in ldiff : go i' j' gs

-- ** Rendering diffs

type ReferenceName = String
type OutputName = String

-- | Generalizes pure strings and terminal output.
--
-- This is really only using a monoid, but 'Applicative' is convenient because
-- there is 'Const' to convert 'Monoid' to 'Applicative', whereas there is no
-- converse defined in @base@.
class Applicative f => Renderer f where
  renderDiff :: Diff Text -> f ()
  renderLine :: String -> f ()

-- | 'Diff' eliminator.
unDiff :: a -> a -> a -> (a -> l -> m) -> Diff l -> m
unDiff x y z f = \case
  First  l -> f x l
  Second l -> f y l
  Both l _ -> f z l

-- | Meant to be @'Const' (DList String)@, which is isomorphic to @[String]@
-- (lists of lines).
newtype ConstString a = ConstString (Const (Endo [String]) a)
  deriving (Functor, Applicative)

-- | 'ConstString' constructor.
constString :: String -> ConstString a
constString = ConstString . Const . Endo . (:)

-- | 'ConstString' destructor.
unConstString :: ConstString a -> String
unConstString (ConstString (Const (Endo f))) = unlines (f [])

instance Renderer ConstString where
  renderDiff = fmap constString . unDiff '-' '+' ' ' $ \c l ->
    c : Text.unpack l
  renderLine = constString

-- * Terminal rendering

-- | We try to respect @tasty@'s output options.
--
-- We will need to smuggle these out of @tasty@ to be able to run our renderer
-- after all tests are finished.
data SneakyOptions = SneakyOptions
  { quiet :: Bool
  , colors :: Bool -> Bool
    -- ^ Parameterized by @isTerm@. Partial application of
    -- 'Test.Tasty.Ingredients.ConsoleReporter.useColor'.
  }

defaultSneakyOptions :: SneakyOptions
defaultSneakyOptions = SneakyOptions
  { quiet = False
  , colors = id
  }

-- | An IO wrapper for colored terminal output.
--
-- > TermIO a = SneakyOptions -> IO a
newtype TermIO a = TermIO (ReaderT SneakyOptions IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | 'TermIO' constructor.
termIO :: (SneakyOptions -> IO a) -> TermIO a
termIO = TermIO . ReaderT

-- | 'TermIO' destructor.
runTerm :: SneakyOptions -> TermIO a -> IO a
runTerm opts (TermIO m) = runReaderT m opts

instance Renderer TermIO where
  renderDiff = unDiff ([red], '-') ([green], '+') ([], ' ') $ \(s, c) l ->
    verbosely . withColor s . liftIO $ do
      putChar c
      Text.putStrLn l
  renderLine = verbosely . liftIO . putStrLn

-- | Color setting.
red, green :: SGR
red   = SetColor Foreground Dull Red
green = SetColor Foreground Dull Green

-- | Check the 'Quiet' flag.
verbosely :: TermIO () -> TermIO ()
verbosely m = do
  opts <- TermIO ask
  unless (quiet opts) m

-- | Check the 'UseColor' flag.
withColor :: [SGR] -> TermIO () -> TermIO ()
withColor s m = do
  opts <- TermIO ask
  isTerm <- liftIO (hSupportsANSI stdout)
  if colors opts isTerm then
    liftIO (setSGR s) >> m >> liftIO (setSGR [])
  else
    m

-- |
--
-- === _Example_
--
-- > renderHeader "Crypto" "Monad"
--
-- Output:
--
-- > --- Crypto
-- > +++ Monad
renderHeader
  :: Renderer f => ReferenceName -> OutputName -> f ()
renderHeader rname oname =
  renderLine ("--- " ++ rname) *>
  renderLine ("+++ " ++ oname)

-- |
--
-- === _Example_
--
-- > renderLocatedDiff LocatedDiff{startL=1, endL=2, startR=3, endR=4, ..}
--
-- Output:
--
-- > @@ -1,2 +3,4 @@
-- > ...
renderLocatedDiff :: Renderer f => LocatedDiff Text -> f ()
renderLocatedDiff LocatedDiff{..} =
  renderLine
    ("@@ -" ++ show startL ++ "," ++ show endL ++
       " +" ++ show startR ++ "," ++ show endR ++ " @@") *>
  traverse_ renderDiff (flattenDiff diffGroup)

-- | Render a contextually-chunked diff. Does nothing if the files don't
-- differ.
renderDiffGroups
  :: Renderer f
  => ReferenceName -> OutputName -> DiffGroups Text -> f ()
renderDiffGroups rname oname gs =
  case filter (not . isTrivialDiff . diffGroup) (locateDiffs gs) of
    [] -> pure ()
    groups ->
      renderHeader rname oname *>
      traverse_ renderLocatedDiff groups

-- | Render a diff with some context.
renderGroupedDiff
  :: Renderer f
  => CtxLen -> ReferenceName -> OutputName -> [Diff [Text]] -> f ()
renderGroupedDiff ctxLen rname oname =
  renderDiffGroups rname oname . contextDiff ctxLen

-- | Render a diff with some context, as a 'String'.
renderGroupedDiffString
  :: CtxLen -> ReferenceName -> OutputName -> [Diff [Text]] -> String
renderGroupedDiffString ctxLen rname oname =
  unConstString . renderGroupedDiff ctxLen rname oname

-- | Print a diff with some context in the terminal.
printGroupedDiff
  :: CtxLen -> ReferenceName -> OutputName -> [Diff [Text]] -> IO ()
printGroupedDiff ctxLen rname oname =
  runTerm defaultSneakyOptions . renderGroupedDiff ctxLen rname oname
