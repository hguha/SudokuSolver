import Data.Array
type Location = (Int, Int)
type Board = Array Location Int

solver :: Board -> [Board]
solver b = solveHelper (findEmpties b) b

solveHelper :: [Location] -> Board -> [Board]
solveHelper [] b = [b]
solveHelper (x:xs) b = concatMap (solveHelper xs) candidateBoards
  where
    candidates = [m | m <- [1..9], isPossible m x b]
    candidateBoards = map (\m -> newBoard m x b) candidates

findEmpties :: Board -> [Location]
findEmpties b = [(row, col) | row <- [0..8], col <- [0..8], b ! (row, col) == 0]

marksInRow :: Board -> Int -> [Int]
marksInRow b row = [b ! loc | loc <- range((row, 0), (row, 8))]

marksInColumn ::  Board -> Int -> [Int]
marksInColumn b col = [b ! loc | loc <- range((0, col), (8, col))]

marksInBox :: Board -> Location -> [Int]
marksInBox b (row, col) = [b ! loc | loc <- locations]
  where
    row' = (div row 3) * 3
    col' = (div col 3) * 3
    locations = range((row', col'), (row' + 2, col' + 2))

isPossible :: Int -> Location -> Board -> Bool
isPossible m (row, col) b = notElem m (marksInRow b row) && notElem m (marksInColumn b col) && notElem m (marksInBox b (row, col))

newBoard :: Int -> Location -> Board -> Board
newBoard mark (row, col) b = b // [((row, col), mark)]

mapPuzzle :: [[Int]] -> [(Location, Int)]
mapPuzzle = concatMap mapRows . zip [0..8]

mapRows :: (Int, [Int]) -> [((Int, Int), Int)]
mapRows (row, marks) = mapCols row (zip [0..8] marks)

mapCols :: Int -> [(Int, Int)] -> [((Int, Int), Int)]
mapCols row cols = map (\(col, m) -> ((row, col), m)) cols

isThereASolution :: [a] -> Maybe a
isThereASolution [] = Nothing
isThereASolution (x:xs) = Just x

printBoard :: Maybe Board -> IO ()
printBoard Nothing  = putStrLn "No solution"
printBoard (Just b) = mapM_ putStrLn [show (marksInRow b row) | row <- [0..8]]

readFromFile :: String -> IO [[Int]]
readFromFile str = readFile str >>= \file -> return $ map ((map $ \x -> read x::Int) . words $) $ lines file

createBoard x = array ((0, 0), (8, 8)) (mapPuzzle x)

solve fileName = do
  x <- readFromFile fileName
  let solution = isThereASolution (solver (createBoard x))
  printBoard solution