summ n = sumnd n
        where sumnd n = nd' n
              nd' 1 = 0
              nd' 0 = 0
              nd' y = if (prime y == True) then y + nd' (y - 1)
                                                    else nd' (y - 1)
              prime x = if prime' x == (x - 1) then True
                            else False
                            where
                              prime' 0 = 0
                              prime' y | mod x (y - 1) > 0 = 1 + prime' (y - 1)
                                       | otherwise = 0
                              mod x y | (y > x) = x
                                      | y == 0 = 0
                                      | otherwise = if y > 1 then mod (x-y) y
                                        else 1
--sumprime = sum primes google
 -- where
  --  primes = 2 : filter isprime [3,5..2000000]
     --   where
      --    isprime n = all ((/= 0).(mod n)) (takeWhile ((<=n).(^2)) primes)

