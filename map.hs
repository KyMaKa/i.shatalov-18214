map' f xs = foldr (\x ys -> f x : ys) [] xs
