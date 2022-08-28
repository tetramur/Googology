module p where
type Row = [Int]

maxb :: Row -> Int
maxb s = length s - length (takeWhile (> last s - 1) (reverse s)) - 1

g :: Row -> Row
g s = take (maxb s) s

b :: Row -> Row
b s = drop (maxb s) (init s)

prss :: Row -> Int -> Int
prss [] n = n
prss s n
  | last s == 0 = prss (init s) (n^2)
  | otherwise = prss (g s ++ take ((n^2+1)*length (b s)) (cycle (b s))) (n^2)
  
main = print $ prss [0..9] 9
