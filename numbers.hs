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
