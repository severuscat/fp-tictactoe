module XO.Server
  ( runServer
  , initApp
  ) where

import XO.Game
import XO.Api
import Servant.Server (Application, serve)
import Control.Concurrent (MVar, newMVar, modifyMVar, readMVar)
import Servant (Server, Handler, (:<|>)(..))
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)

-- | State of the server
type ServerState = [(Board, MarkXO)]

-- | Run server
runServer :: Int -> IO ()
runServer port = run port =<< initApp

-- | Init new server application
initApp :: IO Application
initApp = do
  state <- newMVar []
  return $ createApp state

-- | Create server application
createApp :: MVar ServerState -> Application
createApp state = serve xoApi $ server state

-- | Server logic
server :: MVar ServerState -> Server XOApi
server state = new
          :<|> step
          :<|> replay
  where
    new :: Handler IdAndBoard
    new = liftIO $
      modifyMVar state $ \s ->
        return ((emptyBoard, X) : s, (length s, emptyBoard))

    step :: Int -> Int -> Int -> Handler ResultAndBoard
    step playerId row column = liftIO $ do
      (board, playerMark) <- getGame playerId <$> readMVar state
      let answer =
            case setIfEmpty playerMark (row, column) board of
              Nothing -> (Nothing, board)
              Just board_ ->
                case findResult board_ of
                  jResult@(Just _) ->  (jResult, board_)
                  Nothing -> do
                    let board__ = botStep (botMark playerMark) board_
                    (findResult board__, board__)
      modifyMVar state $ \s ->
        return (setAt_ (length s - 1 - playerId) s (snd answer, playerMark), answer)

    replay :: Int -> Bool -> Handler (Maybe Board)
    replay playerId needReplay
      | not needReplay = pure Nothing
      | otherwise = liftIO $ do
          newMark <- botMark. snd . getGame playerId <$> readMVar state
          let newBoard =
                case newMark of
                  X -> emptyBoard
                  O -> botStep X emptyBoard
          modifyMVar state $ \s ->
            return (setAt_ (length s - 1 - playerId) s (newBoard, newMark), Just newBoard)

-- | Get game data by player id
getGame :: Int -> ServerState -> (Board, MarkXO)
getGame playerId state = state !! (length state - 1 - playerId)

-- | Set value to the board if cell is empty
setIfEmpty :: MarkXO -> Position -> Board -> Maybe Board
setIfEmpty mark p board
  | isEmptyCell $ board !!-!! p = Just $ setAt p (Just mark) board
  | otherwise                   = Nothing

-- | Revert player mark
botMark :: MarkXO -> MarkXO
botMark X = O
botMark O = X