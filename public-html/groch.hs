import Data.Char
r    = [103,11,-3,-12,5,5,-12,11,-62,65,3,-11]
fl s = foldl (\a x -> (head a + x):a) [head s] (tail s)
fr s = reverse $ map (\x -> chr (fromIntegral x)) $ fl s
main = do
    putStrLn $ fr r

