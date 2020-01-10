import qualified Data.List as List
import Debug.Trace
import System.Random
import Data.List (sortBy)

generateRandomSeq :: StdGen -> Int -> (Int,Int) -> ([Int] , StdGen)
generateRandomSeq g 0 _ = ([],g)
generateRandomSeq g l (a,b) =
  let (r, g')   = randomR (a,b) g
      (rt, g'') = generateRandomSeq g' (l-1) (a,b) in
    (r:rt, g'')

shuffleList :: StdGen -> [a] -> ([a], StdGen)
shuffleList g l =
  let (rseq, g') = generateRandomSeq g (length l) (0, 99 * length l) in
  (map snd $ sortBy (\a b -> (fst a) `compare` (fst b)) $ zip rseq l, g')

type Cell = (Int, Int)
data Conn = Conn Cell Cell deriving (Show)

instance Eq Conn where
  Conn a b == Conn c d = ((a==c && b==d) || (a==d && b==c))


data Maze = Maze {width::Int, height::Int,
                  connections::[Conn]} deriving Show

--change the way your maze looks!

vwall :: Maze -> Cell -> Bool
vwall maze (x, y) =
  not $ (Conn (x, y-1) (x, y)) `elem` connections maze


hwall :: Maze -> Cell -> Bool
hwall maze (x, y) =
  not $ (Conn (x-1, y) (x, y)) `elem` connections maze


cellString :: Maze -> Cell -> [String]
cellString maze cell =
  --b=0
  let s = (if(vwall maze cell)
           then "+---"
           else "    " )
  in [s, (if hwall maze cell then "|" else " ") ++ "   "]

mazeRowString :: Maze -> Int -> String
mazeRowString maze col =
   let lines = map (foldl (++) "")
         $ List.transpose
         [cellString maze (c, col) | c <- [0..(width maze)]] in
   foldr (\a b -> a ++ "\n" ++ b) "" (map (take (4 * width maze + 1)) lines)


printMaze :: Maze -> IO ()
printMaze maze = do
  sequence [putStr $ mazeRowString maze row
                | row <- [0..(height maze-1)] ];
  putStrLn ['-' | _ <- [0..(4*width maze)]]
------------------------------------------------

isUntouched :: Maze -> Cell -> Bool
isUntouched m c = not . any (==c) $ foldl (\acc (Conn c1 c2) -> c1:c2:acc) [] (connections m)

getNeighbours :: Maze -> Cell -> [Cell]
getNeighbours m cc = [(x,y) | i <- [-1..1], j <- [-1..1], i == 0 || j == 0, not (i == 0 && j == 0),
                      let x = i + fst cc ; y = j + snd cc,
                          0 <= x, x < width m, 0 <= y, y < height m]

addConn :: Maze -> Cell -> Cell -> Maze
addConn (Maze w h conns) c1 c2 = Maze w h $ (Conn c1 c2):conns

-- Maze, current cell, touched cells -> Maze, touched cells
rbMaze :: StdGen -> Maze -> Cell -> (Maze, StdGen)
rbMaze g m cc = 
  let
    nghbrs = getNeighbours m cc
    (shuff, g') = shuffleList g nghbrs in
    foldl (\(mazeAcum, g'') nc -> if (isUntouched mazeAcum nc)
                           then rbMaze g'' (addConn mazeAcum nc cc) nc
                           else (mazeAcum, g'')) (m, g') shuff
  


main = do
  putStrLn "Enter width of maze : "
  w <- getLine
  putStrLn "Enter height of maze : "
  h <- getLine
  g <- newStdGen
  let generated = rbMaze g (Maze (read w) (read h) []) (0,0)
  printMaze $ fst generated

