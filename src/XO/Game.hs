{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}

module XO.Game
  ( MarkXO(..)
  , Cell
  , Board
  , FinaResult(..)
  , Position
  
  , isEmptyCell
  , inCell
  
  , emptyBoard
  , correctBoard
  , withCheckBoard
  
  , setAt
  , setAt_
  
  , findResult
  , botStep
  
  , (<$$>)
  , ($<$>)
  , (!!-!!)
  ) where

import Data.Maybe (isNothing, fromJust)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Monoid (First(..))


-- | Mars of the board
data MarkXO = X | O
  deriving (Eq, Show, Generic)

instance ToJSON   MarkXO
instance FromJSON MarkXO

type Cell = Maybe MarkXO

-- | Type of the board
type Board = [[Cell]]

-- | Create empty board
emptyBoard :: Board
emptyBoard = replicate 3 $ replicate 3 Nothing

-- | Check that given cell is empty
isEmptyCell :: Maybe MarkXO -> Bool
isEmptyCell = isNothing

-- | Check that given value in given value
inCell :: MarkXO -> Maybe MarkXO -> Bool
inCell expected (Just mark) = expected == mark
inCell _        _           = False

-- | Type of position
type Position = (Int, Int)

-- | Get value from 2d matrix
infixl 9 !!-!!
(!!-!!) :: [[a]] -> Position -> a
board !!-!! (row, column) = board !! row !! column

-- | Result of the game
data FinaResult
  = Win MarkXO  -- ^ Win
  | Mirror      -- ^ Mirror
  deriving (Generic)

instance ToJSON   FinaResult
instance FromJSON FinaResult

instance Show FinaResult where
  show :: FinaResult -> String
  show = \case
    Win X  -> "Player X win!"
    Win O  -> "Player O win!"
    Mirror -> "Mirror"

-- | Check, that board has correct size
correctBoard :: Board -> Bool
correctBoard board =
  length board == 3 && (length <$> board) == [3, 3, 3]

-- | Throw error if board has incorrect size
withCheckBoard :: Board -> r -> r
withCheckBoard board r
  | correctBoard board = r
  | otherwise          = error "Incorrect board size"

-- | 'fmap' for 2d matrix
infixr 5 <$$>
(<$$>) :: (Functor rows, Functor cells) => (a -> b) -> rows (cells a) -> rows (cells b)
f <$$> board = (f <$>) <$> board

-- | Double apply function for 2d matrix
infixr 5 $<$>
($<$>) :: Functor f => (f a -> a) -> f (f a) -> a
f $<$> board = f $ f <$> board

-- | Set value to the given position in 2d matrix
setAt :: Position -> a -> [[a]] -> [[a]]
setAt (row, column) value board = setAt_ row board $ setAt_ column (board !! row) value

-- | Set value to the given position in list
setAt_ :: Int -> [a] -> a -> [a]
setAt_ i ls a
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_:xs) = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []

-- | Check board on result
findResult :: Board -> Maybe FinaResult
findResult board = withCheckBoard board findResult_
  where
    findResult_ :: Maybe FinaResult
    findResult_
      | isMirror   = Just   Mirror
      | isWinner X = Just $ Win X
      | isWinner O = Just $ Win O
      | otherwise  = Nothing

    isMirror :: Bool
    isMirror = and $<$> isEmptyCell <$$> board

    isWinner :: MarkXO -> Bool
    isWinner mark = or $ and <$> inCell mark . (board !!-!!) <$$> winList

    winList :: [[Position]]
    winList =
      [ [(0, 0), (0, 1), (0, 2)]
      , [(1, 0), (1, 1), (1, 2)]
      , [(2, 0), (2, 1), (2, 2)]

      , [(0, 0), (1, 0), (2, 0)]
      , [(0, 1), (1, 1), (2, 1)]
      , [(0, 2), (1, 2), (2, 2)]

      , [(0, 0), (1, 1), (2, 2)]
      , [(2, 0), (1, 1), (0, 2)]
      ]

-- | Bot move
botStep :: MarkXO -> Board -> Board
botStep mark board = do
  let checkPosition :: Position -> First Position
      checkPosition = \case
        p | isEmptyCell (board !!-!! p) -> First $ Just p
          | otherwise                   -> First   Nothing
      position = fromJust . getFirst . mconcat $ checkPosition <$> steps
  setAt position (Just mark) board
  where
    steps :: [Position]
    steps = [(1, ), (2, ), (0, )] <*> [1, 0, 2]
