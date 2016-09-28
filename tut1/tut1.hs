toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
doubleEveryOther :: [Integer] -> [Integer]
sumDigits :: [Integer] -> Integer
validate :: Integer -> Bool

-- Exercise 1 --

toDigits 0 = []

toDigits num =
    if num < 0
        then
            []
        else
            toDigits (num `div` 10) ++ [(num `mod` 10)]

toDigitsRev num =
    reverse (toDigits num)

-- Exercise 2 --

doubleEveryOther digits = reverse (doubleEveryOtherFromLeft (reverse digits))

doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft (x:[]) = [x]
doubleEveryOtherFromLeft (x:ys) = x : (head ys * 2) : doubleEveryOtherFromLeft (tail ys)

-- Exercise 3 --

allToDigits [] = []
allToDigits (a:bs) = toDigits a ++ allToDigits bs

sumDigits digits = sum (allToDigits digits)

-- Exercise 4 --

validate val =
    if (sumDigits (doubleEveryOther (toDigits val))) `mod` 10 == 0
        then
            True
        else
            False
