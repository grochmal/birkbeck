switch :: (a -> b -> c) -> b -> a -> c
switch f x y = f y x

recons :: [a] -> a -> [a]
recons = switch (:)

myrev :: [b] -> [b]
myrev = foldl recons []

-- Easier using lambda
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

