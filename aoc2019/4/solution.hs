import Data.Monoid
import Data.List (group)

increasing :: Int -> Int -> [Int] -> [[Int]]
increasing 0 _ acc = [reverse acc]
increasing n c acc = concat [increasing (n-1) x (x:acc) | x <- [c..9]]

inrange is = is >= [1,2,4,0,7,5] && is <= [5,8,0,7,6,9]
pair is = any (uncurry (==)) $ zip is (tail is)
pair2 = any ((== 2) . length) . group
part1 = [inrange, pair]
part2 = [inrange, pair2]
solution = length $ filter valid $ increasing 6 0 []
  where
    valid = getAll . mconcat (map (All .) part2)

main = print solution
