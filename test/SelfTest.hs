{-# LANGUAGE OverloadedStrings #-}

module SelfTest where

import System.Environment

import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText

import System.Process.Typed

import Test.Tasty

import Test.Tasty.Diff

selfTest :: String -> FilePath -> IO () -> IO ()
selfTest name golden main' = do
  args <- getArgs
  case args of
    "--test" : args' -> withArgs args' main'
    _ -> do
      exe <- getExecutablePath
      let alterEgo = proc exe ("--test" : "--color" : "never" : args)
      mainWithHook $ \hook -> defaultMain $
        goldenTest hook name golden $ do
          (exit, out, err) <- readProcess alterEgo
          return . LText.toStrict $ LText.unlines
            [ LText.pack (show exit)
            , "..."
            , LText.decodeUtf8 out
            , "..."
            , LText.decodeUtf8 err
            ]
