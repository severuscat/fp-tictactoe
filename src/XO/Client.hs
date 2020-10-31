module XO.Client
  ( runGame
  
  , new
  , step
  , replay
  ) where

import Servant.Client (ClientM, client, mkClientEnv, BaseUrl(..), Scheme(Http), runClientM)
import Servant ((:<|>)(..))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Foldable (forM_)

import XO.Game
import XO.Api
import XO.Printer

-- | new request signature
new :: ClientM IdAndBoard

-- | step request signature
step :: Int -> Int -> Int -> ClientM ResultAndBoard

-- | replay request signature
replay :: Int -> Bool -> ClientM (Maybe Board)

-- | Creating requests
new :<|> step :<|> replay = client xoApi

-- | Run game for client
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
        p@(row, column) <- getUserSet board
        if not $ isEmptyCell (board !!-!! p)
        then showMessage "Non empty cell" >> runGame_ board
        else do
          (mbResult, newBoard) <- request $ step playerId row column
          case mbResult of
            Nothing     -> runGame_ newBoard
            Just result -> do
              printBoard (-1, -1) newBoard
              showMessage (show result)
              decision <- askUser "Do you whant to replay?" "Yes" "No"
              mbBoard <- request $ replay playerId decision
              forM_ mbBoard runGame_
  
  prepareConsole
  runGame_ initialBoard