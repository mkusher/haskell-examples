merge :: [Int] -> [Int] -> [Int]
merge [] b = b
merge a [] = a
merge a b | head a >= head b = [head b] ++ (merge a (tail b))
          | True = [head a] ++ (merge (tail a) b)

mergeSort :: [Int] -> [Int]
mergeSort list | length list < 2 = list
               | True = let(a,b) = splitAt (div (length list) 2) list in (merge (mergeSort a) (mergeSort b))