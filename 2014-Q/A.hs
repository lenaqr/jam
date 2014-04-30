import Control.Monad
import Text.Printf
import Data.List (intersect)

readWords :: Read a => IO [a]
readWords = getLine >>= mapM readIO . words

type Case = (Int, [[Int]], Int, [[Int]])

solve :: Case -> String
solve (a, xs, b, ys) =
  let x = xs !! pred a
      y = ys !! pred b
      z = intersect x y
  in case z of
    [ans] -> show ans
    [] -> "Volunteer cheated!"
    _ -> "Bad magician!"

readCase :: IO Case
readCase = do
  a <- readLn
  xs <- replicateM 4 $ readWords
  b <- readLn
  ys <- replicateM 4 $ readWords
  return (a, xs, b, ys)

printCase :: Int -> String -> IO ()
printCase x s = printf "Case #%d: %s\n" x s

main :: IO ()
main = do
  t <- readLn
  cases <- replicateM t readCase
  zipWithM_ printCase [1..] $ map solve cases
