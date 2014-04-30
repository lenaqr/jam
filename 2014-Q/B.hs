import Control.Monad
import Text.Printf

readWords :: Read a => IO [a]
readWords = getLine >>= mapM readIO . words

type Case = (Double, Double, Double)

solve :: Case -> String
solve (c, f, x) =
  let click time prod =
        if x/(prod + f) < (x - c)/prod
        then click (time + c/prod) (prod + f)
        else time + x/prod
  in show $ click 0 2

readCase :: IO Case
readCase = do
  [c, f, x] <- readWords
  return (c, f, x)

printCase :: Int -> String -> IO ()
printCase x s = printf "Case #%d: %s\n" x s

main :: IO ()
main = do
  t <- readLn
  cases <- replicateM t readCase
  zipWithM_ printCase [1..] $ map solve cases
