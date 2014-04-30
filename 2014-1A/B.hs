import Control.Monad
import Text.Printf
import Data.List (delete, sort)
import Data.Map (Map)
import qualified Data.Map as Map

readWords :: Read a => IO [a]
readWords = getLine >>= mapM readIO . words

type Case = (Int, [(Int, Int)])

type AdjacencyList = Map Int [Int]
newtype Tree = Node [Tree] deriving (Show)

readCase :: IO Case
readCase = do
  n <- readLn
  edges <- replicateM (n-1) $ do
    [x, y] <- readWords
    return (x, y)
  return (n, edges)

makeAdjacencyList :: Case -> AdjacencyList
makeAdjacencyList (n, edges) =
  Map.fromListWith (++) $ concat [ [(x, [y]), (y, [x])] | (x, y) <- edges ]

findAdjacent :: Int -> AdjacencyList -> [Int]
findAdjacent = Map.findWithDefault []

rootTreeAt :: Int -> AdjacencyList -> Tree
rootTreeAt n adjacencyList = recur n (-1) where
  recur node parent =
    Node [recur child node | child <- delete parent (findAdjacent node adjacencyList)]

treeScore :: Tree -> Int
treeScore (Node []) = 1
treeScore (Node [x]) = 1
treeScore (Node xs) = 1 + (sum . take 2 . sort) (map treeScore xs)

bestTreeScore :: Int -> AdjacencyList ->  Int
bestTreeScore n adjacencyList = maximum [treeScore (rootTreeAt k adjacencyList) | k <- [1..n]]

solve :: Case -> String
solve (n, edges) =
  let adjacencyList = makeAdjacencyList (n, edges)
  in show $ n - bestTreeScore n adjacencyList

printCase :: Int -> String -> IO ()
printCase x s = printf "Case #%d: %s\n" x s

main :: IO ()
main = do
  t <- readLn
  cases <- replicateM t readCase
  zipWithM_ printCase [1..] $ map solve cases
