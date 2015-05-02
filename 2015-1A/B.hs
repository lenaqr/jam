import Control.Monad
import Text.Printf
import Data.Set (Set)
import qualified Data.Set as Set

readWords :: Read a => IO [a]
readWords = getLine >>= mapM readIO . words

type Case = (Int, Int, [Int])
type Answer = Int

solveSlow :: Case -> Answer
solveSlow (_, n, ms) = helper (n-1) (Set.fromList [(0, i, m) | (m, i) <- zip ms [1..]])

helper :: Int -> Set (Int, Int, Int) -> Int
helper n barbers =
  let ((t, i, m), barbers') = Set.deleteFindMin barbers
  in if n == 0
     then i
     else helper (n-1) (Set.insert (t+m, i, m) barbers')

estimateTime :: Case -> Int
estimateTime (b, n, ms) = floor $ fromIntegral (n - b) / sum (map (recip . fromIntegral) ms)

solve :: Case -> Answer
solve (b, n, ms) =
  let t = estimateTime (b, n, ms)
      n' = n - sum [div t m | m <- ms]
  in helper (n'-1) (Set.fromList [(m * div t m, i, m) | (m, i) <- zip ms [1..]])

readCase :: IO Case
readCase = do
  [b, n] <- readWords
  ms <- readWords
  return (b, n, ms)

printCase :: Int -> Answer -> IO ()
printCase x y = printf "Case #%d: %d\n" x y

main :: IO ()
main = do
  t <- readLn
  cases <- replicateM t readCase
  zipWithM_ printCase [1..] $ map solve cases
