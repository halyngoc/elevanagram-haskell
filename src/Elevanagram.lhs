> module Elevanagram where

> import Data.List
> import System.Random
> import Criterion.Main

This problem was adapted from Google coding competition Kick Start 2019, round
H. Link: https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050edd/00000000001a286d

***********
* PROBLEM *
***********

It is a well known fact that a number is divisible by 11 if and only if the
alternating sum of its digits is equal to 0 modulo 11. For example, 8174958 is
a multiple of 11, since 8 - 1 + 7 - 4 + 9 - 5 + 8 = 22.

Given a number N that consists of digits from 1-9, can you rearrange the digits
to create a number that is divisible by 11?

Since the number might be quite large, you are given integers A1, A2, ..., A9.
There are A_i digits i in the number N, for all i.

Input: List of nine integers [A1, A2, ..., A9] where 1 <= A1 + A2 + ... + A9
and 0 <= A_i, for all i.

Output: True if the digits can be rearranged to create a multiple of 11, and
False otherwise.

**********
* SAMPLE *
**********

> sample1 = [0,0,2,0,0,1,0,0,0] :: [Int]

True
The digits are 336, which can be rearranged to 363. This is a multiple of 11
since 3 - 6 + 3 = 0.

> sample2 = [0,0,0,0,0,0,0,0,12] :: [Int]

True
The digits are 999999999999, which is already a multiple of 11, since 9 - 9 + 9
- 9 + ... - 9 = 0.

> sample3 = [0,0,0,0,2,0,1,1,0] :: [Int]

False
The digits are 5578, which cannot be rearranged to form a multiple of 11.

> sample4 = [3,1,1,1,0,0,0,0,0] :: [Int]

True
The digits are 111234, which can be rearranged to 142131. This is a multiple of
11 since 1 - 4 + 2 - 1 + 3 - 1 = 0.

> sample5 = [3,0,0,0,0,0,3,0,2] :: [Int]

True
The digits are 11177799, which can be rearranged to 19191777. This is a
multiple of 11 since 1 - 9 + 1 - 9 + 1 - 7 + 7 - 7 = -22 (which is 0 modulo
11).

> sample6 = [0,0,0,0,0,0,0,1,0] :: [Int]

False
The only digit is 8, which cannot be rearranged to form a multiple of 11.

Here's a function to generate a sample with equal numbers of A_i.

> makeSample :: Int -> [Int]
> makeSample i = [i,i,i,i,i,i,i,i,i]

> sample7 = makeSample 9

And here's a function to generate a sample with random numbers up to a limit.

> makeRandomSample :: RandomGen g => g -> Int -> [Int]
> makeRandomSample g i = take 9 (randomRs (0, i) g)

******************
* IMPLEMENTATION *
******************

First, we have a function that takes a list of A_is and returns the digits it
represents. Remember that A_i is the number of digits i.

> allDigits = [1,2,3,4,5,6,7,8,9] :: [Int]

> reverseZip :: [a] -> [b] -> [(b, a)]
> reverseZip [] [] = []
> reverseZip (a:as) (b:bs) = [(b, a)] <> (reverseZip as bs)

> repeatInTuple :: (Int, a) -> [a]
> repeatInTuple (i, a) = replicate i a

> getDigits :: [Int] -> [Int]
> getDigits = concat . map repeatInTuple . reverseZip allDigits

Now let's check out some implementations of the problem.

This is the brute force method. It checks all permutations of our input for a
multiple of 11.

> digitsToNumber :: [Int] -> Int
> digitsToNumber = read . concat . map show

> bruteForce :: [Int] -> Bool
> bruteForce = any (\x -> x `mod` 11 == 0) . map digitsToNumber . permutations
>              . getDigits

Let's change this up by using the alternating sum property shown in the problem
statement above.

> alternatingSum :: [Int] -> Int
> alternatingSum [] = 0
> alternatingSum [x] = x
> alternatingSum (x1:x2:xs) = x1 - x2 + (alternatingSum xs)

> isMultipleOf11 :: [Int] -> Bool
> isMultipleOf11 x = (alternatingSum x) `mod` 11 == 0

> bruteForceAltSum :: [Int] -> Bool
> bruteForceAltSum = any isMultipleOf11 . permutations . getDigits

These next solutions use the shortcut shown in the coding competition analysis
section. It says: "If there are at least two numbers >= 10 or at least three
numbers >= 6, we can return YES immediately. Otherwise, there are at most one
value >= 10, and at most two values >= 6, we can calculate all possible
situations, where in the worst case time complexity is O(67 * 10) = O(2799360)"
For a detailed explanation of why it works, read the analysis from the coding
competition page.

> countIf :: (a -> Bool) -> [a] -> Int
> countIf f = length . filter f

> shortcut :: ([Int] -> Bool) -> [Int] -> Bool
> shortcut f xs
>   | countIf (>= 10) xs >= 2 = True
>   | countIf (>= 6)  xs >= 3 = True
>   | otherwise               = f xs

> example1 = shortcut bruteForce sample1
> example2 = shortcut bruteForceAltSum sample7

****************
* BENCHMARKING *
****************

> benchmarkMain = do
>   g <- newStdGen
>   let smallSample = [2,0,0,0,2,3,1,2,0]
>   let bigSample = [4,2,6,1,2,7,1,2,6]
>   putStr "small sample: "
>   print smallSample
>   putStr "big sample: "
>   print bigSample
>   defaultMain
>     [
>       bgroup "A_i <= 5"
>       [
>         bench "shortcut bruteForce" (whnf (shortcut bruteForce) smallSample),
>         bench "shortcut bruteForceAltSum" (whnf (shortcut bruteForceAltSum) smallSample),
>         bench "bruteForce" (whnf bruteForce smallSample),
>         bench "bruteForceAltSum" (whnf bruteForceAltSum smallSample)
>       ],
>       bgroup "A_i <= 10"
>       [
>         bench "shortcut bruteForce" (whnf (shortcut bruteForce) bigSample),
>         bench "shortcut bruteForceAltSum" (whnf (shortcut bruteForceAltSum) bigSample),
>         bench "bruteForce" (whnf bruteForce bigSample),
>         bench "bruteForceAltSum" (whnf bruteForceAltSum bigSample)
>       ]
>     ]
