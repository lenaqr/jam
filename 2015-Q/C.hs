import Control.Monad
import Control.Applicative
import Text.Printf

readWords :: Read a => IO [a]
readWords = getLine >>= mapM readIO . words

type Case = (Int, Int, String)
type Answer = String

quaternionAccum :: (Bool, Char) -> Char -> (Bool, Char)
quaternionAccum (n, c1) c2
  | c1 == '1' = (n, c2)
  | c1 == c2  = (not n, '1')
  | c1 == 'i' && c2 == 'j' = (n, 'k')
  | c1 == 'j' && c2 == 'k' = (n, 'i')
  | c1 == 'k' && c2 == 'i' = (n, 'j')
  | c1 == 'i' && c2 == 'k' = (not n, 'j')
  | c1 == 'k' && c2 == 'j' = (not n, 'i')
  | c1 == 'j' && c2 == 'i' = (not n, 'k')

consumePrefix :: Char -> String -> Maybe String
consumePrefix c1 = f (True, c1) where
  f (False, '1') cs = Just cs
  f nc (c:cs) = f (quaternionAccum nc c) cs
  f _ [] = Nothing

checkNull :: String -> Bool
checkNull = f (False, '1') where
  f (False, '1') [] = True
  f _ [] = False
  f nc (c:cs) = f (quaternionAccum nc c) cs

solve :: Case -> String
solve (_, x, cs) =
  let cs' = concat (replicate x cs)
      rest = Just cs' >>= consumePrefix 'i' >>= consumePrefix 'j' >>= consumePrefix 'k'
      ans = checkNull <$> rest
  in case ans of
    Just True -> "YES"
    _ -> "NO"

consumePrefix' :: Char -> (String, Int, String) -> Maybe (String, Int, String)
consumePrefix' c1 (suffix, x, cs) =
  do let x' = min x 4
     suffix' <- consumePrefix c1 (suffix ++ concat (replicate x' cs))
     return (suffix', x - x', cs)

checkNull' :: (String, Int, String) -> Bool
checkNull' (suffix, x, cs) =
  let x' = mod x 4
  in checkNull (suffix ++ concat (replicate x' cs))

solve' :: Case -> String
solve' (_, x, cs) =
  let cs' = ("", x, cs)
      rest = Just cs' >>= consumePrefix' 'i' >>= consumePrefix' 'j' >>= consumePrefix' 'k'
      ans = checkNull' <$> rest
  in case ans of
    Just True -> "YES"
    _ -> "NO"

readCase :: IO Case
readCase = do
  [l, x] <- readWords
  cs <- getLine
  return (l, x, cs)

printCase :: Int -> Answer -> IO ()
printCase = printf "Case #%d: %s\n"

main :: IO ()
main = do
  t <- readLn
  cases <- replicateM t readCase
  zipWithM_ printCase [1..] $ map solve' cases
