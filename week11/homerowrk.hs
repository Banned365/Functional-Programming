listToNumber :: [Int] -> Int
listToNumber [] = 0
listToNumber (x:[]) = x
listToNumber xs = result 0 xs
    where
        result res(x:[]) = res * 10 + x
        result res (x:xs) = result (res * 10 + x) xs


suffix :: [Int] -> [Int] -> Bool
suffix fir sec 
    |len < 0 = False
    |len > 0 = drop len sec == fir
        where
            len = length sec - length fir


occurrences :: [Int] -> [Int] -> [Int]
occurrences [] l2 = []
occurrences l1 [] = map zer l1
    where
        zer x = x * 0
occurrences l1 l2 = check (head l1) l2 : occurrences (tail l1) l2
    where
        check n [] = 0
        check n list
            | n == head list = 1 + check n (tail list)
            |otherwise = check n (tail list)



removeAt n [] = error "The list is empty"
removeAt 0 ys = tail ys
removeAt n ys
    |n > length ys && n < 0 = error "Integer out of bounds!"
    |otherwise = iter (ys !! n) ys 
        where
            iter number [] = []
            iter number xs
                |not (number == head xs) = head xs : iter number (tail xs)
                |otherwise = iter number (tail xs)

