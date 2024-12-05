import Data.Vector ((!),(!?))
import qualified Data.Vector as V

getAllLines :: FilePath -> IO [String]
getAllLines path = do
  content <- readFile path
  pure $ lines content

toVector :: [String] -> V.Vector (V.Vector Char)
toVector x = V.fromListN (length x) $ map (\x -> V.fromListN (length x) x) x

checkWord :: String -> V.Vector (V.Vector Char) -> Int -> Int -> (Int -> Int) -> (Int -> Int) ->Bool
checkWord s v x y dx dy = f s x y True
  where f [] _ _ b = b
        f (s:xs) x y b = case v !? y of
                           Just l -> b && f xs (dx x) (dy y) (maybe False (s==) (l !? x))
                           Nothing -> False

checkHorizontal :: String -> V.Vector (V.Vector Char) -> Int -> Int -> Bool
checkHorizontal s v x y = checkWord s v x y (+1) id

checkVertical :: String -> V.Vector (V.Vector Char) -> Int -> Int -> Bool
checkVertical s v x y = checkWord s v x y id (+1)

checkDownDiag :: String -> V.Vector (V.Vector Char) -> Int -> Int -> Bool
checkDownDiag s v x y = checkWord s v x y (+1) (+1)

checkUpDiag :: String -> V.Vector (V.Vector Char) -> Int -> Int -> Bool
checkUpDiag s v x y = checkWord s v x y (+1) (subtract 1)

walkGrid :: V.Vector (V.Vector Char) -> Int
walkGrid g = f 0 0
  where ymax = length g
        xmax = length (g ! 0)
        f x y | x < xmax && y < ymax = f (x + 1) y
                                       + fromEnum (checkHorizontal "XMAS" g x y || checkHorizontal "SAMX" g x y)
                                       + fromEnum (checkVertical "XMAS" g x y || checkVertical "SAMX" g x y)
                                       + fromEnum (checkDownDiag "XMAS" g x y || checkDownDiag "SAMX" g x y)
                                       + fromEnum (checkUpDiag "XMAS" g x y || checkUpDiag "SAMX" g x y)
              | x == xmax = f 0 (y + 1)
              | y == ymax = 0
                                       

final1 fp = toVector  <$> getAllLines fp
