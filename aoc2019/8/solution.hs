import Data.Maybe (fromJust)
import Data.List (elemIndex, transpose, intercalate)

width = 25
height = 6
type Layer = String
black = '0'
white = '1'
trans = '2'

printLayer :: Layer -> String
printLayer l = map trans $ intercalate "\n" rows
  where rows = chunkUp width l
        trans '0' = ' '
        trans '1' = 'X'
        trans x = x

reduce :: [Layer] -> Layer
reduce = map f . transpose
  where
    f = head . filter (/= trans)

layerize :: String -> [Layer]
layerize = chunkUp (width * height)

chunkUp :: Int -> [a] -> [[a]]
chunkUp len = takeWhile (not . null) . map (take len) . iterate (drop len)

numberOf :: Char -> Layer -> Int
numberOf c = length . filter (== c)

main :: IO ()
main = part2 . layerize . head . lines <$> readFile "input" >>= putStrLn
  where
    part1 :: [Layer] -> String
    part1 inp =
      let zeros = map (numberOf '0') inp
          llz = fromJust $ elemIndex (minimum zeros) zeros
          s = inp !! llz
       in show $ numberOf '1' s * numberOf '2' s

    part2 :: [Layer] -> String
    part2 = printLayer . reduce
