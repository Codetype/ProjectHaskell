{-| Module      : Test.HUnits
    Description : This module contains HUnit tests, which ckeck correctness of our code.
                  HUnit tests are similar to well known JUnit from Java.
    Copyright   : (c) Paweł Gędłek, 2018
                      Rafał Cegielski, 2018
    License     : BSD3
    Maintainer  : pawelgedlek@gmail.com
-}
module Test.HUnits where

import Test.HUnit
import Dequeue2 ( Dequeue'(MkDequeue'), emptyDEQ', isEmptyDEQ', lengthDEQ', firstDEQ', lastDEQ', takeFrontDEQ', takeBackDEQ', pushFrontDEQ', popFrontDEQ', pushBackDEQ', popBackDEQ')

testIsEmptyDEQ :: Test
testIsEmptyDEQ =
     TestCase $ assertEqual "Should return True" True (isEmptyDEQ' (MkDequeue' []))

testLengthDEQ :: Test
testLengthDEQ =
     TestCase $ assertEqual "Should return Dequeue length" 10 (lengthDEQ' (MkDequeue' [1..10]))

testFirstDEQ :: Test
testFirstDEQ =
     TestCase $ assertEqual "Should return Just Dequeue first element" (Just 1) (firstDEQ' (MkDequeue' [1..10]))

testLastDEQ :: Test
testLastDEQ =
     TestCase $ assertEqual "Should return Just Dequeue last element" (Just 10) (lastDEQ' (MkDequeue' [1..10]))

testTakeFrontDEQ :: Test
testTakeFrontDEQ =
     TestCase $ assertEqual "Should return 5 first Dequeue elements" [1..5] (takeFrontDEQ' 5 (MkDequeue' [1..10]))

testTakeBackDEQ :: Test
testTakeBackDEQ =
     TestCase $ assertEqual "Should return 5 last Dequeue elements" [6..10] (takeBackDEQ' 5 (MkDequeue' [1..10]))

testPushFrontDEQ :: Test
testPushFrontDEQ =
    TestCase $ assertEqual "Should return Dequeue with new element on the front" (MkDequeue' [0..10]) (pushFrontDEQ' (MkDequeue' [1..10]) 0)

testPopFrontDEQ :: Test
testPopFrontDEQ =
    TestCase $ assertEqual "Should return pair (Dequeue head, Dequeue tail)" (Just (1, MkDequeue' [2..10])) (popFrontDEQ' (MkDequeue' [1..10]))

testPushBackDEQ :: Test
testPushBackDEQ =
    TestCase $ assertEqual "Should return Dequeue with new element on the end" (MkDequeue' [1..11]) (pushBackDEQ' (MkDequeue' [1..10]) 11)

testPopBackDEQ :: Test
testPopBackDEQ =
    TestCase $ assertEqual "Should return Dequeue without last el" (Just (10, MkDequeue' [1..9])) (popBackDEQ' (MkDequeue' [1..10]))

--put below code on comand line
--runTestTT $ TestList [testIsEmptyDEQ, testLengthDEQ, testFirstDEQ, testLastDEQ, testTakeFrontDEQ, testTakeBackDEQ, testPushFrontDEQ, testPopFrontDEQ, testPushBackDEQ, testPopBackDEQ]
