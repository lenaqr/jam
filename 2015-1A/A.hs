import Control.Monad
import Text.Printf

readWords :: Read a => IO [a]
readWords = getLine >>= mapM readIO . words

type Case = (Int, [Int])
type Answer = (Int, Int)

solve :: Case -> Answer
solve (_, counts) = (firstMethod counts, secondMethod counts)

drops :: [Int] -> [Int]
drops (x1:x2:xs) = max 0 (x1 - x2) : drops (x2:xs)
drops _ = []

firstMethod :: [Int] -> Int
firstMethod xs = sum (drops xs)

secondMethod :: [Int] -> Int
secondMethod xs = let m = maximum (drops xs) in sum (map (min m) (init xs))

readCase :: IO Case
readCase = do
  n <- readLn
  ms <- readWords
  return (n, ms)

printCase :: Int -> Answer -> IO ()
printCase x (y, z) = printf "Case #%d: %d %d\n" x y z

main :: IO ()
main = do
  t <- readLn
  cases <- replicateM t readCase
  zipWithM_ printCase [1..] $ map solve cases
