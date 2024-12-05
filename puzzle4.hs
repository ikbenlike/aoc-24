import Data.Vector ((!),(!?))
import qualified Data.Vector as V

getAllLines :: FilePath -> IO [String]
getAllLines path = do
  content <- readFile path
  pure $ lines content

toVector :: [String] -> V.Vector (V.Vector Char)
toVector x = V.fromListN (length x) $ map (\x -> V.fromListN (length x) x) x

{-checkHorizontal :: String -> V.Vector Char -> Int -> Bool
checkHorizontal s v i = f s v i True
  where f [] _ _ b = b
        f (x:xs) v i b = b && f xs v (i + 1) (maybe False (x==) (v !? i))

checkVertical :: String -> V.Vector (V.Vector Char) -> Int -> Int -> Bool
checkVertical s v l i = f s v l True
  where f [] _ _ b = b
        f (x:xs) v l b = case v !? l of
                           Just line -> b && f xs v (l + 1) (maybe False (x==) (line !? i))
                           Nothing -> False-}

--checkWord :: String -> V.Vector (V.Vector Char) -> Int -> Int -> (Int -> Int) -> (Int -> Int) ->Bool
checkWord s v x y dx dy = f s x y True
  where f [] _ _ b = b
        f (s:xs) x y b = case v !? y of
                           Just l -> b && f xs (dx x) (dy y) (maybe False (s==) (l !? x))
                           Nothing -> False

{-checkHorizontal :: String -> V.Vector (V.Vector Char) -> Int -> Int -> Bool
checkHorizontal s v x y = checkWord s v x y (+1) id

checkVertical :: String -> V.Vector (V.Vector Char) -> Int -> Int -> Bool
checkVertical s v x y = checkWord s v x y id (+1)

checkDownDiag :: String -> V.Vector (V.Vector Char) -> Int -> Int -> Bool
checkDownDiag s v x y = checkWord s v x y (+1) (+1)

checkUpDiag :: String -> V.Vector (V.Vector Char) -> Int -> Int -> Bool
checkUpDiag s v x y = checkWord s v x y (+1) ((-) 1)

checkLeftDiag :: String -> V.Vector (V.Vector Char) -> Int -> Int -> Bool
checkLeftDiag s v x y = checkWord s v x y ((-) 1) ((+) 1)-}

final1 fp = toVector  <$> getAllLines fp
