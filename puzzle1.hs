import System.IO
import Control.Monad
import Data.Char
import Data.List

stringToNum :: String -> Int
stringToNum s = f s $ (length s - 1)
  where f [] _ = 0
        f (x:xs) n = digitToInt x * 10 ^ n + f xs (n - 1)

getAllLines :: FilePath -> IO [String]
getAllLines path = do
  content <- readFile path
  pure $ lines content

makeListPair :: [[Int]] -> ([Int], [Int])
makeListPair xs = g $ foldr f ([],[]) xs
  where f (x:y:[]) b = (x:fst b, y:snd b)
        g (a,b) = (sort a, sort b)

difference :: ([Int], [Int]) -> Int
difference ([],_) = 0
difference (_,[]) = 0
difference (x:xs,y:ys) = abs (x - y) + difference (xs, ys)

final1 :: FilePath -> IO Int
final1 fp =
  difference <$> makeListPair <$> map (map stringToNum) <$> map words <$> getAllLines fp

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

score :: ([Int], [Int]) -> Int
score ([],_) = 0
score (_,[]) = 0
score (x:xs,ys) = x * count x ys + score (xs,ys)

final2 :: FilePath -> IO Int
final2 fp =
  score <$> makeListPair <$> map (map stringToNum) <$> map words <$> getAllLines fp
