module Main
  ( main
  ) where

import Test.Tasty (defaultMain)
import Test.Hspec (hspec)

import Game
import Server

main :: IO ()
main = do
  hspec serverSpec
  defaultMain gameTestGroup