{- |
Module                  : Lecture1
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 1 of the Haskell Beginners course.

To complete exercises, you need to complete implementation and add
missing top-level type signatures. You can implement any additional
helper functions. But you can't change the names of the given
functions.

Comments before each function contain explanations and example of
arguments and expected returned values.

It's absolutely okay if you feel that your implementations are not
perfect. You can return to these exercises after future lectures and
improve your solutions if you see any possible improvements.
-}

module Lecture1
    ( makeSnippet
    , sumOfSquares
    , lastDigit
    , minmax
    , subString
    , strSum
    , lowerAndGreater
    ) where

-- VVV If you need to import libraries, do it after this line ... VVV

-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

{- | Specify the type signature of the following function. Think about
its behaviour, possible types for the function arguments and write the
type signature explicitly.

>>> makeSnippet 3 "Test"
"Des..."
-}
makeSnippet:: Int -> String -> String
makeSnippet limit text =
        take  limit ("Description: " ++ text) ++ "..."


{- | Implement a function that takes two numbers and finds sum of
their squares.

>>> sumOfSquares 3 4
25.0

>>> sumOfSquares (-2) 7
53.0

Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
is 25.
-}
sumOfSquares:: Float -> Float -> Float
sumOfSquares x y = x*x + y*y

{- | Implement a function that returns the last digit of a given number.
🕯 HINT: use the @mod@ function

>>> lastDigit 42
2


>>> lastDigit (-17)
7

-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
lastDigit:: Integer -> Integer
lastDigit n = mod (abs n) 10

{- | Write a function that takes three numbers and returns the
difference between the biggest number and the smallest one.

Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
and 1 is the smallest, and 7 - 1 = 6.

Try to use local variables (either let-in or where) to implement this
function.

>>> minmax 7 2 5
5
-}
minmax:: Integer->Integer->Integer->Integer
minmax x y z =
    let
        ma = max x $ max y z
        mi = min x $ min y z
    in
        ma-mi


{- | Implement a function that takes a string, start and end positions
and returns a substring of a given string from the start position to
the end (including).

>>> subString 3 7 "Hello, world!"
"lo, w"

>>> subString 10 5 "Some very long String"
""

>>> subString (-3)  7 "Hello, world!"
"Hello, w"

>>> subString 7 (-3)  "Hello, world!"
""
>>> subString 0 0 "Hello, world!"
"H"

>>> subString (-1) 3 "Hello"
"Hell"

This function can accept negative start and end position. Negative
start position can be considered as zero (e.g. substring from the
first character) and negative end position should result in an empty
string.
-}
subString:: Int -> Int -> String -> String
subString start end str
    | end<0 = ""
    | start<0 = subString 0 end str
    | otherwise = take (end - start  + 1) $ drop start str

{- | Write a function that takes a String — space separated numbers,
and finds a sum of the numbers inside this string.
The string contains only spaces and/or numbers.

>>> strSum "100    -42  15"
73

>>> strSum' "100    -42  15"
73

>>> strSum ""
0
-}

strSum:: String -> Int
strSum = go 0 . words
    where
        go:: Int -> [String] -> Int
        go acc [] = acc
        go acc (w:ws) = go (acc + read w) ws


strSum':: String -> Int
strSum'  = sum . map read . words

{- | Write a function that takes a number and a list of numbers and
returns a string, saying how many elements of the list are strictly
greater than the given number and strictly lower.

Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
The given number 3 is greater than 2 elements (1 and 2)
and lower than 6 elements (4, 5, 6, 7, 8 and 9).

🕯 HINT: Use recursion to implement this function.

>>> lowerAndGreater 3 [1 .. 9]
"3 is greater than 2 elements and lower than 6 elements"

>>> lowerAndGreater 3 []
"3 is greater than 0 elements and lower than 0 elements"

>>> lowerAndGreater 13 [1 .. 9]
"13 is greater than 9 elements and lower than 0 elements"

>>> lowerAndGreater 0 [1 .. 9]
"0 is greater than 0 elements and lower than 9 elements"
-}

lowerAndGreater:: Int -> [Int] -> String
lowerAndGreater n ns = show n ++ " is greater than " ++ show (countItemsLowerThan n ns) ++ " elements and lower than " ++ show (countItemsGreaterThan n ns) ++ " elements"
    where
        countItemsLowerThan = countItemsComparingThan (<)
        countItemsGreaterThan = countItemsComparingThan (>)
        countItemsComparingThan:: (Int->Int->Bool) -> Int -> [Int] -> Int
        countItemsComparingThan _ _ [] = 0
        countItemsComparingThan op n (k:ks)
            |op k n     = 1 + countItemsComparingThan op n ks
            |otherwise  = countItemsComparingThan op n ks
