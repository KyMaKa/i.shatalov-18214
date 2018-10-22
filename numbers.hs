import Data.Char
toDecimal:: Int -> String -> String
toDecimal base snumber | base < 10 = show (toDecimal' 1 snumber)
                       | base > 36 && base < 62 = show (toDecimal''' 1 snumber)
                       | base > 10 && base < 37 = show (toDecimal'' 1 snumber)
                       | otherwise = snumber
          where
            toDecimal' _ "" = 0
            toDecimal' n ys = if (fromEnum (head ys)-48) >= 0 && (fromEnum (head ys)-48) < base then (fromEnum (head ys) - 48)*(base^(length(snumber) - n)) + toDecimal' (n+1) (tail ys)
              else error "wrong argument"
            toDecimal'' _ "" = 0
            toDecimal'' n ys  | fromEnum(head ys) >= 65 && fromEnum(head ys) <= 96 || fromEnum(head ys) - 87 >= base || fromEnum(head ys) < 48 = error "wrong argument"
                              | fromEnum(head ys) >= 97 = (fromEnum (head ys) - 87)*(base^(length(snumber) - n)) + toDecimal'' (n+1) (tail ys)
                              | otherwise = (fromEnum (head ys) - 48)*(base^(length(snumber) - n)) + toDecimal'' (n+1) (tail ys)
            toDecimal''' _ "" = 0
            toDecimal''' n ys | fromEnum(head ys) - 29 >= base && fromEnum(head ys) < 97 || fromEnum(head ys) < 48 = error "wrong argument"
                              | fromEnum(head ys) >= 97 = (fromEnum (head ys) - 87)*(base^(length(snumber) - n)) + toDecimal''' (n+1) (tail ys)
                              | fromEnum(head ys) >= 65 = (fromEnum (head ys) - 29)*(base^(length(snumber) - n)) + toDecimal''' (n+1) (tail ys)
                              | otherwise = (fromEnum (head ys) - 48)*(base^(length(snumber) - n)) + toDecimal''' (n+1) (tail ys)

fromDecimal:: Int -> String -> String
fromDecimal toBase snumber | toBase < 10 = fromDecimal' (read snumber::Int) ""
                           | toBase > 36 = fromDecimal''' (read snumber) ""
                           | toBase > 10 && toBase < 37 = fromDecimal'' (read snumber::Int) ""
                           | otherwise = snumber
  where
    fromDecimal' ys ss | ys >= toBase = fromDecimal' (div ys toBase) (show(mod ys toBase)++ss)
                       | otherwise = show ys ++ ss

    fromDecimal'' 0 ss = ss
    fromDecimal'' ys ss | ys > toBase && ys >= 10 && ys `mod` toBase >= 10 = fromDecimal'' (div ys toBase) ((toEnum((mod ys toBase) + 87)::Char) : ss)
                        | ys < 10 = (show(mod ys toBase)++ss)
                        | ys == toBase || ys > toBase = fromDecimal'' (div ys toBase) (show(mod ys toBase)++ss)
                        | otherwise = ((toEnum(ys + 87)::Char) : ss)

    fromDecimal''' 0 ss = ss
    fromDecimal''' ys ss | ys > toBase && ys >= 10 && ys `mod` toBase >= 10 = fromDecimal''' (div ys toBase) ((toEnum((mod ys toBase) + 87)::Char) : ss)
                         | ys < 10 = (show(mod ys toBase)++ss)
                         | ys == toBase || ys > toBase = fromDecimal''' (div ys toBase) (show(mod ys toBase)++ss)
                         | ys > 35 = fromDecimal''' (div ys toBase) ((toEnum((mod ys toBase) + 29)::Char) : ss)
                         | otherwise = ((toEnum(ys + 87)::Char) : ss)

converFromTo:: Int -> Int -> String -> String
converFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)
