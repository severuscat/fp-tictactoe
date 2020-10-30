{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module XO.Api
  ( IdAndBoard
  , ResultAndBoard
  , XOApi

  , xoApi
  ) where

import Servant (Get, JSON, (:>), (:<|>), Capture, Proxy(..))

import XO.Game

-- | Int and Board
type IdAndBoard = (Int, Board)

-- | Maybe result and board
type ResultAndBoard = (Maybe FinaResult, Board)

-- | Api of the server
type XOApi = "new" :> Get '[JSON] IdAndBoard
        :<|> "step" :> Capture "userid" Int
                    :> Capture "row" Int
                    :> Capture "column" Int
                    :> Get '[JSON] ResultAndBoard
        :<|> "replay" :> Capture "userid" Int
                      :> Capture "decision" Bool
                      :> Get '[JSON] (Maybe Board)

-- | Proxy api of the server
xoApi :: Proxy XOApi
xoApi = Proxy