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
        f x y | x < xmax && y < ymax =
                let helper s f = fromEnum $ f s g x y
                    fns = [checkHorizontal, checkVertical, checkDownDiag, checkUpDiag]
                    d = foldr (+) 0 $ map (helper "XMAS") fns ++ map (helper "SAMX") fns
                in
                  d + f (x + 1) y
              | x == xmax = f 0 (y + 1)
              | y == ymax = 0
                                       

final1 fp = walkGrid <$> toVector  <$> getAllLines fp
