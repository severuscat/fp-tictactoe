module MockClient 
  ( runGame
  ) where

import Servant.Client (ClientM, mkClientEnv, BaseUrl(..), Scheme(Http), runClientM)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Foldable (forM_)
import Control.Exception (throwIO)

import XO.Game
import XO.Client hiding (runGame)

runGame :: Int -> IO ()
runGame port = do
  manager <- newManager defaultManagerSettings
  let request :: ClientM a -> IO a
      request clientM = do
        let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" port "")
        mbA <- runClientM clientM clientEnv
        case mbA of
          Left err -> putStrLn ("Error: " ++ show err) >> request clientM
          Right a  -> return a
  
  (playerId, initialBoard) <- request new
  
  let runGame_ :: Board -> IO ()
      runGame_ board = do
        let p@(row, column) = botStep_ board
        if not $ isEmptyCell (board !!-!! p)
        then throwIO $ userError "Server not undestend position"
        else do
          (mbResult, newBoard) <- request $ step playerId row column
          case mbResult of
            Nothing -> runGame_ newBoard
            Just _  -> do
              mbBoard <- request $ replay playerId False
              forM_ mbBoard runGame_

  runGame_ initialBoard