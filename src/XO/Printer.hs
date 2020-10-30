{-# LANGUAGE LambdaCase #-}

module XO.Printer
  ( printBoard
  , prepareConsole
  , clearConsole
  , askUser
  , showMessage
  , getUserSet
  ) where

import XO.Game
import Data.List (intercalate)
import System.IO (hReady, stdin, hSetBuffering, BufferMode(NoBuffering), hSetEcho)
import Control.Monad (void)
import System.Process (system)

-- | Type of the string board
type StrBoard = [[String]]

-- | Print board to the console
printBoard :: Position -> Board -> IO ()
printBoard sp realBoard = withCheckBoard realBoard $
  printStrBoard . starBoard $ showCell <$$> realBoard
  where
    showCell :: Cell -> String
    showCell = \case
      Nothing -> "     "
      Just X  -> "  X  "
      Just O  -> "  O  "

    starBoard :: StrBoard -> StrBoard
    starBoard board
      | sp == (-1, -1)  = board
      | otherwise       = setAt sp (starCell $ board !!-!! sp) board

    starCell :: String -> String
    starCell = \case
      "     " -> " * * "
      "  X  " -> " *X* "
      "  O  " -> " *O* "
      _     -> error "Impossible case"

    printStrBoard :: StrBoard -> IO ()
    printStrBoard board = do
      let rowSeparator = intercalate "|" (replicate 3 "     ") ++ "\n"
                      ++ intercalate "+" (replicate 3 "-----") ++ "\n"
                      ++ intercalate "|" (replicate 3 "     ") ++ "\n"

      putStrLn $ intercalate "|" (replicate 3 "     ") ++ "\n"
              ++ intercalate rowSeparator ((++ "\n") . intercalate "|" <$> board)
              ++ intercalate "|" (replicate 3 "     ") ++ "\n"

-- | Get pressed key name
getKey :: IO [Char]
getKey = reverse <$> getKey_ ""
  where getKey_ chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey_ else return) (char:chars)

-- | Prepare console buffers for program work
prepareConsole :: IO ()
prepareConsole = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

-- | Clear console
clearConsole :: IO ()
clearConsole = void (system "clear")

-- | Ask user tru/false question
askUser :: String -> String -> String -> IO Bool
askUser question yesW noW = askUser_ False
  where
    askUser_ :: Bool -> IO Bool
    askUser_ yesno = do
      clearConsole

      let yes = if yesno then "*" ++ yesW ++ "*" else " " ++ yesW ++ " "
      let no  = if yesno then " " ++ noW  ++ " " else "*" ++ noW  ++ "*"

      putStrLn question
      putStrLn $ yes ++ "    " ++ no

      key <- getKey
      case key of
        "\ESC[C" -> askUser_ False
        "\ESC[D" -> askUser_ True
        "\n"     -> return   yesno
        _        -> askUser_ yesno

-- | Run interactive getting user step
getUserSet :: Board -> IO Position
getUserSet board = getUserSet_ (0, 0)
  where
    getUserSet_ :: Position -> IO Position
    getUserSet_ p = do
      clearConsole

      printBoard p board
      
      key <- getKey
      case key of
        "\ESC[A" -> getUserSet_ $ up p
        "\ESC[B" -> getUserSet_ $ down p
        "\ESC[C" -> getUserSet_ $ right p
        "\ESC[D" -> getUserSet_ $ left p
        "\n"     -> return p
        _        -> getUserSet_ p
    
    up :: Position -> Position
    up (row, column) =
      (if row == 0 then 0 else row - 1, column)

    down :: Position -> Position
    down (row, column) =
      (if row + 1 == length board then row else row + 1, column)

    left :: Position -> Position
    left (row, column) =
      (row, if column == 0 then 0 else column - 1)

    right :: Position -> Position
    right (row, column) =
      (row, if column + 1 == length (board !! row) then column else column + 1)

-- | Show some message to user
showMessage :: String -> IO ()
showMessage msg = do
  putStrLn msg

  let loop = do 
        key <- getKey
        case key of
          "\n"     -> return ()
          _        -> loop

  loop
