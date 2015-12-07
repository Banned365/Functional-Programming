truncatablePrime :: Int -> Bool
truncatablePrime n
	|n < 10 && isPrime n = True
    |isPrime n = truncatablePrime (div n 10)
    |otherwise = False
    	where
    		isPrime n
    			| n <= 1 = False
    			| otherwise = isPrime' 2 n
        			where
            			isPrime' current n
                			| current == n = True
                			| mod n current == 0 = False
                			| otherwise = isPrime' (current + 1) n


containsDigits :: Int -> Int -> Bool
containsDigits x y
    |y < 10 = mod x 10 == y
    |x < 10 = containsDigits idx (div y 10)
    |mod x 10 == mod y 10 = True
    |otherwise = containsDigits (div x 10) y
        where
            idx = x
            idy = y

productOfDigits :: Int -> Int
productOfDigits 0 = 1
productOfDigits x = (mod x 10) * productOfDigits (div x 10)


quadrant :: Double -> Double -> Int
quadrant x y
    |x < 0 && y < 0 = 3
    |x > 0 && y < 0 = 4
    |x < 0 && y > 0 = 2
    |x > 0 && y > 0 = 1
    |otherwise = 0


