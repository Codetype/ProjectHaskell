{-| Module      : Test.QCheck
    Description : This module contains QuickCheck tests, which ckeck correctness of our code.
                  QuickCheck tests generate many test automatic and check our code.
    Copyright   : (c) Paweł Gędłek, 2018
                      Rafał Cegielski, 2018
    License     : BSD3
    Maintainer  : pawelgedlek@gmail.com
-}

module Test.QCheck where

import Test.QuickCheck
import Dequeue2 ( Dequeue'(MkDequeue'), emptyDEQ', isEmptyDEQ', lengthDEQ', firstDEQ', lastDEQ', takeFrontDEQ', takeBackDEQ', pushFrontDEQ', popFrontDEQ', pushBackDEQ', popBackDEQ')
import Dequeue3 (Dequeue''(Dequeue''), emptyDEQ'', isEmptyDEQ'', lengthDEQ'', firstDEQ'', lastDEQ'', takeFrontDEQ'', takeBackDEQ'', pushFrontDEQ'', popFrontDEQ'', pushBackDEQ'',popBackDEQ'')

instance (Arbitrary a) => Arbitrary (Dequeue' a) where
         arbitrary = do
                     list <- arbitrary
                     return $ MkDequeue' list

prop_isEmptyDEQ'' :: [Int] -> [Int] -> Bool
prop_isEmptyDEQ'' xs ys = isEmptyDEQ'' (Dequeue'' xs ys) == (null xs && null ys)

prop_isEmptyDEQ' :: [Int] -> Bool
prop_isEmptyDEQ' xs = isEmptyDEQ' (MkDequeue' xs) == null xs

prop_lengthDEQ' :: [Int] -> Bool
prop_lengthDEQ' xs = lengthDEQ' (MkDequeue' xs) == length xs

prop_lengthDEQ'' :: [Int] -> [Int] -> Bool
prop_lengthDEQ'' xs ys = lengthDEQ'' (Dequeue'' xs ys) == (length xs + length ys)

prop_firstDEQ' :: [Int] -> Bool
prop_firstDEQ' xs = if null xs
    then firstDEQ' (MkDequeue' xs) == Nothing
    else firstDEQ' (MkDequeue' xs) == Just (head xs)

prop_firstDEQ'' :: [Int] -> [Int] -> Bool
prop_firstDEQ'' xs ys = if null xs
     then firstDEQ'' (Dequeue'' xs ys) == Nothing
     else firstDEQ'' (Dequeue'' xs ys) == Just (head xs)

prop_lastDEQ' :: [Int] -> Bool
prop_lastDEQ' xs = if null xs
    then lastDEQ' (MkDequeue' xs) == Nothing
    else lastDEQ' (MkDequeue' xs) == Just (last xs)

prop_lastDEQ'' :: [Int] -> [Int] -> Bool
prop_lastDEQ'' xs ys = if null ys
     then lastDEQ'' (Dequeue'' xs ys) == Nothing
     else lastDEQ'' (Dequeue'' xs ys) == Just (head ys)

prop_FrontDEQ' :: [Int] -> Int -> Bool
prop_FrontDEQ' xs n = takeFrontDEQ' n (MkDequeue' xs) == take n xs

prop_FrontDEQ'' :: Int -> [Int] -> [Int] -> Bool
prop_FrontDEQ'' n xs ys = takeFrontDEQ'' n (Dequeue'' xs ys) == take n xs

prop_BackDEQ' :: [Int] -> Int -> Bool
prop_BackDEQ' xs n = takeBackDEQ' n (MkDequeue' xs) == drop (length xs - n) xs

prop_BackDEQ'' :: Int -> [Int] -> [Int] -> Bool
prop_BackDEQ'' n xs ys = takeBackDEQ'' n (Dequeue'' xs ys) == take n ys

prop_PushFDEQ' :: [Int] -> Int -> Bool
prop_PushFDEQ' xs n = pushFrontDEQ' (MkDequeue' xs) n == MkDequeue' (n:xs)

prop_PushFDEQ'' :: [Int] -> [Int] -> Int -> Bool
prop_PushFDEQ'' xs ys n = pushFrontDEQ'' (Dequeue'' xs ys) n == Dequeue'' (n:xs) ys

prop_popFrontDEQ' :: [Int] -> Bool
prop_popFrontDEQ' xs = if null xs
     then popFrontDEQ' (MkDequeue' xs) == Nothing
     else popFrontDEQ' (MkDequeue' xs) == Just (head xs, MkDequeue'(tail xs))

prop_popFrontDEQ'' :: [Int] -> [Int] -> Bool
prop_popFrontDEQ'' xs ys = if (null xs && null ys)
     then popFrontDEQ'' (Dequeue'' xs ys) == Nothing
     else if null xs
          then popFrontDEQ'' (Dequeue'' xs ys) == popFrontDEQ'' (Dequeue'' (reverse ys) [])
          else popFrontDEQ'' (Dequeue'' xs ys) == Just (head xs, (Dequeue'' (tail xs) ys))

prop_PushBDEQ' :: [Int] -> Int -> Bool
prop_PushBDEQ' xs n = pushBackDEQ' (MkDequeue' xs) n == MkDequeue' (xs++[n])

prop_PushBDEQ'' :: [Int] -> [Int] -> Int -> Bool
prop_PushBDEQ'' xs ys n = pushBackDEQ'' (Dequeue'' xs ys) n == Dequeue'' xs (n:ys)

prop_popBackDEQ' :: [Int] -> Bool
prop_popBackDEQ' xs = if null xs
     then popBackDEQ' (MkDequeue' xs) == Nothing
     else popBackDEQ' (MkDequeue' xs) == (Just (last xs, MkDequeue' (takeFrontDEQ' ((length xs) - 1) (MkDequeue' xs) )))

prop_popBackDEQ'' :: [Int] -> [Int] -> Bool
prop_popBackDEQ'' xs ys = if (null xs && null ys)
     then popBackDEQ'' (Dequeue'' xs ys) == Nothing
     else if null ys
          then popBackDEQ'' (Dequeue'' xs ys) == popBackDEQ'' (Dequeue'' [] (reverse xs))
          else popBackDEQ'' (Dequeue'' xs ys) == Just (head ys, (Dequeue'' xs (tail ys)))
