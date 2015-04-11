import Control.Monad
import Text.Printf

readWords :: Read a => IO [a]
readWords = getLine >>= mapM readIO . words

type Case = (Int, [Int])
type Answer = String

solve :: Case -> Answer
solve (_, counts) = show (f 0 counts) where
  f _ [] = 0
  f n (s:ss) = max (-n) (f (n+s-1) ss)

readCase :: IO Case
readCase = do
  line <- getLine
  let [smax, digits] = words line
  return (read smax, map (read . (:[])) digits)

printCase :: Int -> Answer -> IO ()
printCase = printf "Case #%d: %s\n"

main :: IO ()
main = do
  t <- readLn
  cases <- replicateM t readCase
  zipWithM_ printCase [1..] $ map solve cases
