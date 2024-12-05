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

data Direction = Increasing | Decreasing
  deriving (Enum,Eq)

increasing :: [Int] -> Bool
increasing (x:[]) = True
increasing (x:y:xs) | 1 <= (y - x) && (y - x) <= 3 = increasing (y:xs)
                    | otherwise = False

safe :: [Int] -> (Int -> Int -> Int) -> Bool
safe (x:[]) _ = True
safe (x:y:xs) f | 1 <= f x y && f x y <= 3 = safe (y:xs) f
                | otherwise = False

safeCount :: ([Int] -> (Int -> Int ->Int) -> Bool) ->  [[Int]] -> Int
safeCount g xs = f xs 0
  where i x y = y - x
        d x y = x - y
        f [] acc = acc
        f (x:xs) acc | g x i || g x d = f xs (acc + 1)
                     | otherwise = f xs acc

final1 :: FilePath -> IO Int
final1 fp = safeCount safe <$> map (map stringToNum) <$> map words <$> getAllLines fp

safeWithRemoval :: [Int] -> (Int -> Int -> Int) -> Bool
safeWithRemoval xs f = g xs [] f
  where g (x:[]) _ _ = True
        g (x:y:xs) ys f | 1 <= f x y && f x y <= 3 = g (y:xs) (ys ++ [x]) f
                        | safe (ys ++ (x:xs)) f = True
                        | safe (ys ++ (y:xs)) f = True
                        | otherwise = False

final2 :: FilePath -> IO Int
final2 fp = safeCount safeWithRemoval <$> map (map stringToNum) <$> map words <$> getAllLines fp
