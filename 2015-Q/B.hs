import Control.Monad
import Text.Printf

readWords :: Read a => IO [a]
readWords = getLine >>= mapM readIO . words

type Case = (Int, [Int])
type Answer = Int

minutes :: [Int] -> Int -> Int
minutes counts p =
  p + sum (map ((`div` p) . subtract 1) counts)

solve :: Case -> Answer
solve (_, counts) = minimum (map (minutes counts) [1..(maximum counts)])

readCase :: IO Case
readCase = do
  d <- readLn
  ps <- readWords
  return (d, ps)

printCase :: Int -> Answer -> IO ()
printCase x y = printf "Case #%d: %s\n" x (show y)

main :: IO ()
main = do
  t <- readLn
  cases <- replicateM t readCase
  zipWithM_ printCase [1..] $ map solve cases
