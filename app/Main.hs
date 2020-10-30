{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import System.Environment (getArgs)

import XO.Client
import XO.Server

main :: IO ()
main = do
  getArgs >>= \case
    ["server", port] -> runServer (read @Int port)
    ["client", port] -> runGame (read @Int port)
    args -> error $ "Invalid arguments" <> show args