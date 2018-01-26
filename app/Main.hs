{-| Module      : Main
    Description : Main module, which imports all the most important modules in our project
                  Main contains two Dequeue implementations with constructors and functions.
                  HUnit and QuickCheck is also executable here.
    Copyright   : (c) Paweł Gędłek, 2018
                      Rafał Cegielski, 2018
    License     : BSD3
    Maintainer  : pawelgedlek@gmail.com
-}

module Main where

-- | Firstly import all my modules.
import Dequeue2 ( Dequeue'(MkDequeue'), emptyDEQ', isEmptyDEQ', lengthDEQ', firstDEQ', lastDEQ', takeFrontDEQ', takeBackDEQ', pushFrontDEQ', popFrontDEQ', pushBackDEQ', popBackDEQ')
import Dequeue3 (Dequeue''(Dequeue''), emptyDEQ'', isEmptyDEQ'', lengthDEQ'', firstDEQ'', lastDEQ'', takeFrontDEQ'', takeBackDEQ'', pushFrontDEQ'', popFrontDEQ'', pushBackDEQ'',popBackDEQ'')
import Test.QCheck
import Test.HUnits

-- | Then import all needed modules
import Text.Printf
import Control.Exception
import System.CPUTime

time :: IO t -> IO t
time a = do
    startTime <- getCPUTime
    v <- a
    endTime   <- getCPUTime
    let diff = (fromIntegral (endTime - startTime)) / (10^12)
    printf "Algorithm time: %0.9f sec\n" (diff :: Double)
    return v

-- main functions
main = do
    putStrLn "Start"
    measureTimeEmptyDEQ
    putStrLn "End."

-- | function measures time of all Dequeue2 functions
measureTime :: Num a => Dequeue' a -> IO()
measureTime x = do
     putStrLn "Test time of function execution:"
     putStr "> emptyDEQ'\t"
     time $ emptyDEQ' `seq` return()
     putStr "> isEmptyDEQ'\t"
     time $ isEmptyDEQ' x `seq` return()
     putStr "> lengthDEQ'\t"
     time $ lengthDEQ' x `seq` return()
     putStr "> firstDEQ'\t"
     time $ firstDEQ' x `seq` return()
     putStr "> lastDEQ'\t"
     time $ lastDEQ' x `seq` return()
     putStr "> takeFrontDEQ' "
     time $ takeFrontDEQ' 100 x `seq` return()
     putStr "> takeBackDEQ'  "
     time $ takeBackDEQ' 100 x `seq` return()
     putStr "> pushFrontDEQ' "
     time $ pushFrontDEQ' x 0  `seq` return()
     putStr "> popFrontDEQ'  "
     time $ popFrontDEQ' x   `seq` return()
     putStr "> pushBackDEQ'  "
     time $ pushBackDEQ' x 0  `seq` return()
     putStr "> popBackDEQ'   "
     time $ popBackDEQ' x `seq` return()

-- | function measures time of emptyDEQ implementations
measureTimeEmptyDEQ :: IO()
measureTimeEmptyDEQ = do
     putStrLn "Test time of function execution:"
     putStr "> emptyDEQ''\t"
     time $ emptyDEQ' `seq` return()
     putStr "> emptyDEQ'\t"
     time $ emptyDEQ'' `seq` return()

-- | function measures time of isEmptyDEQ implementations
measureTimeIsEmptyDEQ :: Dequeue' a -> Dequeue'' a -> IO()
measureTimeIsEmptyDEQ q1 q2 = do
     putStrLn "Test time of function execution:"
     putStr "> isEmptyDEQ''\t"
     time $ isEmptyDEQ'' q2 `seq` return()
     putStr "> isEmptyDEQ'\t"
     time $ isEmptyDEQ' q1 `seq` return()

-- | function measures time of lengthDEQ implementations
measureTimeLengthDEQ :: Dequeue' a -> Dequeue'' a -> IO()
measureTimeLengthDEQ q1 q2 = do
     putStrLn "Test time of function execution:"
     putStr "> lengthDEQ''\t"
     time $ lengthDEQ'' q2 `seq` return()
     putStr "> lengthDEQ'\t"
     time $ lengthDEQ' q1 `seq` return()

-- | function measures time of firstDEQ implementations
measureTimeFirstDEQ :: Dequeue' a -> Dequeue'' a -> IO()
measureTimeFirstDEQ q1 q2 = do
     putStrLn "Test time of function execution:"
     putStr "> firstDEQ''\t"
     time $ firstDEQ'' q2 `seq` return()
     putStr "> firstDEQ'\t"
     time $ firstDEQ' q1 `seq` return()

-- | function measures time of lastDEQ implementations
measureTimeLastDEQ :: Dequeue' a -> Dequeue'' a -> IO()
measureTimeLastDEQ q1 q2 = do
     putStrLn "Test time of function execution:"
     putStr "> lastDEQ''\t"
     time $ lastDEQ'' q2 `seq` return()
     putStr "> lastDEQ'\t"
     time $ lastDEQ' q1 `seq` return()

-- | function measures time of frontDEQ implementations
measureTimeFrontDEQ :: Dequeue' a -> Dequeue'' a -> IO()
measureTimeFrontDEQ q1 q2 = do
     putStrLn "Test time of function execution:"
     putStr "> takeFrontDEQ'' "
     time $ takeFrontDEQ'' 100 q2 `seq` return()
     putStr "> takeFrontDEQ'  "
     time $ takeFrontDEQ' 100 q1 `seq` return()

-- | function measures time of backDEQ implementations
measureTimeBackDEQ :: Dequeue' a -> Dequeue'' a -> IO()
measureTimeBackDEQ q1 q2 = do
     putStrLn "Test time of function execution:"
     putStr "> takeBackDEQ'' "
     time $ takeBackDEQ'' 100 q2 `seq` return()
     putStr "> takeBackDEQ'  "
     time $ takeBackDEQ' 100 q1 `seq` return()

-- | function measures time of pushFrontDEQ implementations
measureTimePushFrontDEQ :: Num a => Dequeue' a -> Dequeue'' a -> IO()
measureTimePushFrontDEQ q1 q2 = do
     putStrLn "Test time of function execution:"
     putStr "> pushFrontDEQ'' "
     time $ pushFrontDEQ'' q2 0  `seq` return()
     putStr "> pushFrontDEQ'  "
     time $ pushFrontDEQ' q1 0  `seq` return()

-- | function measures time of popFrontDEQ implementations
measureTimePopFrontDEQ :: Dequeue' a -> Dequeue'' a -> IO()
measureTimePopFrontDEQ q1 q2 = do
     putStrLn "Test time of function execution:"
     putStr "> popFrontDEQ'' "
     time $ popFrontDEQ'' q2   `seq` return()
     putStr "> popFrontDEQ'  "
     time $ popFrontDEQ' q1   `seq` return()

-- | function measures time of pushBackDEQ implementation
measureTimePushBackDEQ :: Num a => Dequeue' a -> Dequeue'' a -> IO()
measureTimePushBackDEQ q1 q2 = do
     putStrLn "Test time of function execution:"
     putStr "> pushBackDEQ'' "
     time $ pushBackDEQ'' q2 0  `seq` return()
     putStr "> pushBackDEQ'  "
     time $ pushBackDEQ' q1 0  `seq` return()

-- | function measures time of pushBackDEQ implementations
measureTimePopBackDEQ :: Dequeue' a -> Dequeue'' a -> IO()
measureTimePopBackDEQ q1 q2 = do
     putStrLn "Test time of function execution:"
     putStr "> popBackDEQ''  "
     time $ popBackDEQ'' q2 `seq` return()
     putStr "> popBackDEQ'   "
     time $ popBackDEQ' q1 `seq` return()
