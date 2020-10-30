{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module XO.Api where

import Servant (Get, JSON, (:>), (:<|>), Capture, Proxy(..))

import XO.Game

type IdAndBoard = (Int, Board)

type ResultAndBoard = (Maybe FinaResult, Board)

type XOApi = "new" :> Get '[JSON] IdAndBoard
        :<|> "step" :> Capture "userid" Int
                    :> Capture "row" Int
                    :> Capture "column" Int
                    :> Get '[JSON] ResultAndBoard
        :<|> "replay" :> Capture "userid" Int
                      :> Capture "decision" Bool
                      :> Get '[JSON] (Maybe Board)

xoApi :: Proxy XOApi
xoApi = Proxy