calc :: Int -> Int
calc m = m `div` 3 - 2

part1 :: [Int] -> Int
part1 = sum . map calc

part2 :: [Int] -> Int
part2 = sum . concatMap (takeWhile (>0) . tail . iterate calc)

main :: IO ()
main = show . part2 . map read . lines <$> readFile "input" >>= putStrLn
