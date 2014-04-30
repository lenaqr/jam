import Control.Monad
import Text.Printf
import Data.List (sort)

readWords :: Read a => IO [a]
readWords = getLine >>= mapM readIO . words

type Case = (Int, [Double], [Double])

war :: Int -> [Double] -> [Double] -> Int
war 0 _ _ = 0
war n (x:xs) (y:ys)
  | x > y = 1 + war (n-1) (x:xs) ys
  | x < y = war (n-1) xs ys
  | otherwise = error "Equal weights"

deceitful :: Int -> [Double] -> [Double] -> Int
deceitful n xs ys = n - war n ys xs

solve :: Case -> String
solve (n, naomiBlocks, kenBlocks) =
  let naomiSorted = sort naomiBlocks
      kenSorted = sort kenBlocks
      y = deceitful n naomiSorted kenSorted
      z = war n naomiSorted kenSorted
  in printf "%d %d" y z

readCase :: IO Case
readCase = do
  n <- readLn
  naomiBlocks <- readWords
  kenBlocks <- readWords
  return (n, naomiBlocks, kenBlocks)

printCase :: Int -> String -> IO ()
printCase x s = printf "Case #%d: %s\n" x s

main :: IO ()
main = do
  t <- readLn
  cases <- replicateM t readCase
  zipWithM_ printCase [1..] $ map solve cases
