merge :: [Int] -> [Int] -> [Int]
merge [] b = b
merge a [] = a
merge (x:a) (y:b) | x >= y = [y] ++ (merge ([x]++a) b)
                  | True = [x] ++ (merge a ([y]++b))

mergeSort :: [Int] -> [Int]
mergeSort list | length list < 2 = list
               | True = let(a,b) = splitAt (div (length list) 2) list in (merge (mergeSort a) (mergeSort b))