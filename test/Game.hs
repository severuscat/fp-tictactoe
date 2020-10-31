module Game
  ( gameTestGroup
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import XO.Game

gameTestGroup :: TestTree
gameTestGroup = testGroup "Test game"
  [ testGroup "isEmptyCell"
      [ testCase "True" $
          isEmptyCell Nothing @?= True
      , testCase "False (X)" $
          isEmptyCell (Just X) @?= False
      , testCase "False (O)" $
          isEmptyCell (Just O) @?= False
      ]
  , testGroup "inCell"
      [ testCase "True X" $
          inCell X (Just X) @?= True
      , testCase "True O" $
          inCell O (Just O) @?= True
      , testCase "False X O" $ do
          inCell X (Just O) @?= False
          inCell O (Just X) @?= False
      , testCase "False Nothing" $ do
          inCell X Nothing @?= False
          inCell O Nothing @?= False
      ]
  , testCase "(!!-!!)" $ do
      let matrix = [[1], [2, 3], [4]] :: [[Int]]
      matrix !!-!! (0, 0) @?= 1
      matrix !!-!! (1, 0) @?= 2
      matrix !!-!! (1, 1) @?= 3
      matrix !!-!! (2, 0) @?= 4
  , testGroup "correctBoard"
      [ testCase "True" $
          correctBoard emptyBoard @?= True
      , testCase "False empty board" $
          correctBoard [] @?= False
      , testCase "False empty rows" $
          correctBoard (replicate 3 []) @?= False
      , testCase "False incorrect size" $
          correctBoard (replicate 4 $ replicate 3 Nothing) @?= False
      ]
  , testCase "(<$$>)" $ do
      let board = replicate 3 <$> [ Just X, Nothing, Just O]
      isEmptyCell <$$> board @?= (isEmptyCell <$>) <$> board
  , testCase "($<$>)" $ do
      let matrix = replicate 4 <$> [True, True, False]
      (and $<$> matrix) @?= and (and <$> matrix)
  , testCase "setAt" $ do
      let matrix = [[0, 0], [0, 0]] :: [[Int]]
      let expected1 = [[1, 0], [0, 0]]
      let expected2 = [[1, 0], [0, 2]]
      let actual1 = setAt (0, 0) 1 matrix
      let actual2 = setAt (1, 1) 2 actual1
      actual1 @?= expected1
      actual2 @?= expected2
  , testCase "setAt_" $ do
      let list = [0, 0, 0, 0] :: [Int]
      let expected1 = [1, 0, 0, 0]
      let expected2 = [1, 0, 2, 0]
      let actual1 = setAt_ 0 list 1
      let actual2 = setAt_ 2 actual1 2
      actual1 @?= expected1
      actual2 @?= expected2
  , testGroup "findResult"
      [ testCase "Find Mirror" $ do
          let board = Just <$$>
                [ [ O, O, X ]
                , [ X, X, O ]
                , [ O, X, X ]
                ]
          findResult board @?= Just Mirror
      , testCase "Find Win X" $ do
          let board = Just <$$>
                [ [ O, X, O ]
                , [ X, X, O ]
                , [ O, X, X ]
                ]
          findResult board @?= Just (Win X)
      , testCase "Find Win O" $ do
          let board =
                [ [ Just X,  Just O, Just X  ]
                , [ Just X,  Just O, Just X  ]
                , [ Nothing, Just O, Nothing ]
                ]
          findResult board @?= Just (Win O)
      , testCase "No result" $
          findResult emptyBoard @?= Nothing
      ]
  , testCase "botStep" $ do
      let beforeBoard =
            [ [ Just X,  Just O,  Just X  ]
            , [ Just X,  Just O,  Just X  ]
            , [ Nothing, Nothing, Nothing ]
            ]
      let afterBoard = botStep O beforeBoard
      correctBoard afterBoard @?= True
      beforeBoard /= afterBoard @?= True
      let countOf :: MarkXO -> Board -> Int
          countOf mark board = length $ filter (inCell mark) $ concat board
      countOf X beforeBoard == countOf X afterBoard @?= True
      countOf O beforeBoard == countOf O afterBoard - 1 @?= True
  ]