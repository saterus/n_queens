-- EightQueens.hs
module EightQueens where

import Control.Monad(forM)
import System.Random(StdGen, randomRs, newStdGen)
import Data.List(intersperse, deleteBy, minimum, sort)

numTrials = 5 -- 50

-- 1 queen per column, with the value representing the row
type Column = (Int, Int)
type Board = [Column]
type Solution = [Int]

-- board parameters
boardSize, minRow, maxRow, minColumn, maxColumn :: Int
boardSize = 8
minRow = 0
maxRow = boardSize - 1
minColumn = 0
maxColumn = boardSize - 1

possibleMoves :: [Column]
possibleMoves = let rowValues = [minRow..maxRow]
                in [(x,y) | x <- rowValues, y <- rowValues]
main = do
  boards <- forM [0..numTrials] $ \a -> do
    gen <- newStdGen
    let board = genRandomBoard gen
    return board
  mapM_ (putStrLn . boardString) boards
  return boards

genRandomBoard :: StdGen -> Board
genRandomBoard gen = zip [0..] $ take boardSize $ randomRs (minRow, maxRow) gen

calculateCost :: Board -> Int -> Int -> Int
calculateCost b n i = undefined

-- Check horizontal cost of test column 'n' against all other columns
horizCost :: Board -> Column -> Int
horizCost b n = length $ filter (inRow n) b

-- Checks to see if n is the same row as i
-- but queens in the same vertical column shouldn't be compared
inRow :: Column -> Column -> Bool
inRow (nc,nr) (ic,ir)
  | nc == ic  = False
  | otherwise = nr == ir

diagCost :: Board -> Column -> Int
diagCost b n = length $ filter (inDiagonal n) b

inDiagonal :: Column -> Column -> Bool
inDiagonal (nc,nr) (ic,ir)
  | nc == ic  = False
  | otherwise = (abs (nr - ir)) == (abs (ic - nc))

calcCost :: Board -> Column -> Int
calcCost b n = (horizCost b n) + (diagCost b n)

totalCost :: Board -> Int
totalCost b = (((flip div) 2) . sum) $ map (calcCost b) b

findBestMove :: Board -> Column
findBestMove b = snd $ findLowestCost b

findLowestCost :: Board -> (Int, Column)
findLowestCost b = minimum $ costColumnPairs b

costColumnPairs :: Board -> [(Int, Column)]
costColumnPairs b = map (pairs b) possibleMoves

pairs :: Board -> Column -> (Int, Column)
pairs b column = (totalCost (moveQueen b column), column)

findLowest :: (Int, Column) -> (Int, Column) -> (Int, Column)
findLowest best@(bCost, _) current@(cCost, _)
  | cCost < bCost = current
  | otherwise     = best

moveQueen :: Board -> Column -> Board
moveQueen b newColumn = let b' = deleteBy (\c i-> (fst c) == (fst i)) newColumn b
                        in newColumn : b'

makeMove :: Board -> Board
makeMove b = let newColumn = findBestMove b
                 b' = deleteBy (\c i-> (fst c) == (fst i)) newColumn b
             in newColumn : b'

isGoalState :: Board -> Bool
isGoalState = (== 0) . totalCost

onShoulder :: Board -> Bool
onShoulder b = (totalCost b) == ((fst .findLowestCost) b)

atMaximum :: Board -> Bool
atMaximum b = (isGoalState b) || (onShoulder b)

hillClimb :: Board -> Board
hillClimb b
  | atMaximum b = b
  | otherwise   = hillClimb $ makeMove b

hillClimbWithTrail :: Board -> IO Board
hillClimbWithTrail b
  | atMaximum b = do
      printBoard b
      return b
  | otherwise   = do
      printMove b
      hillClimbWithTrail $ makeMove b

printBoard = putStrLn . boardString

printCostPairs b = mapM_ (putStrLn . show) ((sort . costColumnPairs) b)

printMove b =  printBoard b >> putStrLn ("Best Move " ++ (show (findBestMove b)))

-- To simply things, the board is printed with columns going left to right.
boardString :: Board -> String
boardString b = bar ++ (concatMap rows (zip [0..] (sort b))) ++ bar ++ cost ++ "\n"
  where lineWidth = (2 + boardSize * 2)
        surround c v before after = replicate before c ++ v ++ replicate after c
        cost = let v = "Cost: " ++ (show (totalCost b)) in surround '-' v 1 (lineWidth - (length v) - 1)
        bar = (replicate lineWidth '-') ++ "\n"
        rows :: (Int, Column) -> String
        rows (i, (_,r)) = (show i) ++ "|" ++ (intersperse '|' (queen r)) ++ "|\n"
          where queen :: Int -> [Char]
                queen row = surround ' ' "Q" row (maxRow - row)



