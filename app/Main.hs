{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import System.Environment (getArgs)

import XO.Client
import XO.Server

-- | Main point of the application
main :: IO ()
main = do
  getArgs >>= \case
    ["server", port] -> runServer (read @Int port)
    ["client", port] -> runGame (read @Int port)
    args -> error $ "Invalid arguments" <> show args