{-# LANGUAGE LambdaCase #-}

module Server
  ( serverSpec
  ) where

import Test.Hspec (Spec, describe, around, it)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Control.Concurrent.Async (async, waitCatch)
import Control.Monad (replicateM, (>=>), forM_)

import XO.Server
import MockClient

serverSpec :: Spec
serverSpec =
  describe "Test server" $ do
      around withUserApp $
        it "Connect single client" runGame
      around withUserApp $
        it "Connect 228 clients" $ \port -> do
          clients <- replicateM 228 (async $ runGame port)
          forM_ clients $
            waitCatch >=> \case
              Right _ -> return ()
              Left  e -> ioError $ userError $ show e
            

withUserApp :: (Port -> IO ()) -> IO ()
withUserApp = testWithApplication initApp