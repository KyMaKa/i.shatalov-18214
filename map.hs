map' f xs = foldr (\x ys -> f x : ys) [] xs
map'' f xs = foldl (\ys x -> ys ++ [f x]) [] xs
