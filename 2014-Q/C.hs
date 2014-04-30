import Control.Monad
import Text.Printf

readWords :: Read a => IO [a]
readWords = getLine >>= mapM readIO . words

type Case = (Int, Int, Int)

solve :: Case -> String
solve (r, c, m) = showGrid c $ placeMines r c m c

placeMines :: Int -> Int -> Int -> Int -> Maybe [Int]
placeMines r c m d
  | m == 0    = Just $ replicate r 0
  | m > d * r = Nothing
  | r /= 2 && d >= m       && 1 /= c - m                  = Just $ m : replicate (r-1) 0
  | r == 2 && d >= div m 2 && 1 /= c - div m 2 && even m  = Just $ [div m 2, div m 2]
  | r == 2 && d * 2 - 1 == m && c == d                    = Just $ [d, d - 1]
  | r <= 2       = Nothing
  | otherwise = msum [ fmap (i:) $ placeMines (r-1) c (m-i) i
                     | i <- reverse [1..(min d m)], 1 /= c - i ]

showGrid :: Int -> Maybe [Int] -> String
showGrid c Nothing = "Impossible\n"
showGrid c (Just xs) = let showLine x = replicate (c - x) '.' ++ replicate x '*'
                           b:bs = unlines . map showLine $ reverse xs
                       in 'c':bs

readCase :: IO Case
readCase = do
  [c, f, x] <- readWords
  return (c, f, x)

printCase :: Int -> String -> IO ()
printCase x s = printf "Case #%d:\n" x >> putStr s

main :: IO ()
main = do
  t <- readLn
  cases <- replicateM t readCase
  zipWithM_ printCase [1..] $ map solve cases
