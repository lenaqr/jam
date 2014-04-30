import Control.Monad
import Text.Printf
import qualified Data.Map as Map

readWords :: Read a => IO [a]
readWords = getLine >>= mapM readIO . words

type Bits = [Bool]
type Case = (Int, Int, [Bits], [Bits])

xor :: Bits -> Bits -> Bits
xor = zipWith (/=)

bitcount :: Bits -> Int
bitcount = length . filter id

frequenciesOf :: Ord a => [a] -> [(a, Int)]
frequenciesOf xs = Map.toList . Map.fromListWith (+) $ zip xs (repeat 1)

solve :: Case -> String
solve (n, l, outlets, devices) =
  let allPairs = [ xor outlet device | outlet <- outlets, device <- devices ]
      candidates = [ bitcount x | (x, k) <- frequenciesOf allPairs, k == n ]
  in case candidates of
    [] -> "NOT POSSIBLE"
    _  -> show $ minimum candidates

readBits :: String -> Bits
readBits s = map (== '1') s

readCase :: IO Case
readCase = do
  [a, b] <- readWords
  outlets <- getLine
  devices <- getLine
  return (a, b,
          map readBits $ words outlets,
          map readBits $ words devices)

printCase :: Int -> String -> IO ()
printCase x s = printf "Case #%d: %s\n" x s

main :: IO ()
main = do
  t <- readLn
  cases <- replicateM t readCase
  zipWithM_ printCase [1..] $ map solve cases
