{-| Module      : Dequeue3
    Description : This module contains Dequeue ADT implementation with definition of the constructor
                  and functions, which let make some operations on it.
    Copyright   : (c) Paweł Gędłek, 2018
                      Rafał Cegielski, 2018
    License     : BSD3
    Maintainer  : pawelgedlek@gmail.com
-}

module Dequeue3
  (  Dequeue''(Dequeue'')
  ,  emptyDEQ''       -- :: Dequeue a
  ,  isEmptyDEQ''     -- :: Dequeue a -> Bool
  ,  lengthDEQ''      -- :: Dequeue a -> Int
  ,  firstDEQ''       -- :: Dequeue a -> Maybe a
  ,  lastDEQ''        -- :: Dequeue a -> Maybe a
  ,  takeFrontDEQ''   -- :: Int -> Dequeue a -> [a]
  ,  takeBackDEQ''    -- :: Int -> Dequeue a -> [a]
  ,  pushFrontDEQ''   -- :: Dequeue a -> a -> Dequeue a
  ,  popFrontDEQ''    -- :: Dequeue a -> Maybe(a, Dequeue a)
  ,  pushBackDEQ''    -- :: Dequeue a -> a -> Dequeue a
  ,  popBackDEQ''     -- :: Dequeue a -> Maybe(a, Dequeue a)
  ) where

--interface (signature, contract)

emptyDEQ''     :: Dequeue'' a
isEmptyDEQ''   :: Dequeue'' a -> Bool
lengthDEQ''    :: Dequeue'' a -> Int
firstDEQ''     :: Dequeue'' a -> Maybe a
lastDEQ''      :: Dequeue'' a -> Maybe a
takeFrontDEQ'' :: Int -> Dequeue'' a -> [a]
takeBackDEQ''  :: Int -> Dequeue'' a -> [a]
pushFrontDEQ'' :: Dequeue'' a -> a -> Dequeue'' a
popFrontDEQ''  :: Dequeue'' a -> Maybe(a, Dequeue'' a)
pushBackDEQ''  :: Dequeue'' a -> a -> Dequeue'' a
popBackDEQ''   :: Dequeue'' a -> Maybe(a, Dequeue'' a)

--implementation
-- | Dequeue' data constructor
data Dequeue'' a = Dequeue'' [a] [a] deriving (Eq, Show)

-- | function create empty Dequeue' instance
emptyDEQ'' = Dequeue'' [] []

-- | function check, is Dequeue' is empty
isEmptyDEQ'' (Dequeue'' inb out) = null inb && null out

-- | function return length of Dequeue'
lengthDEQ'' (Dequeue'' inb out) = length inb + length out

-- | function return first element of Dequeue'
firstDEQ'' (Dequeue'' inb out) = if null inb && null out
          then Nothing
          else if null inb
               then Just (last out)
               else Just (head inb)

-- | function return last element of Dequeue'
lastDEQ'' (Dequeue'' [] []) = Nothing
lastDEQ'' (Dequeue'' inb []) = Just (last inb)
lastDEQ'' (Dequeue'' inb out) = Just (head out)

-- | function return first n elements from Dequeue'
takeFrontDEQ'' x (Dequeue'' [] []) = []
takeFrontDEQ'' x (Dequeue'' [] out) = takeBackDEQ'' x (Dequeue'' [] out)
takeFrontDEQ'' x (Dequeue'' inb _) = take x inb

-- | function return last n elements from Dequeue'
takeBackDEQ'' x (Dequeue'' [] []) = []
takeBackDEQ'' x (Dequeue'' inb []) = takeFrontDEQ'' x (Dequeue'' inb [])
takeBackDEQ'' x (Dequeue'' _ out) = take x out

-- | function add element on front of Dequeue'
pushFrontDEQ'' (Dequeue'' inb out) e = Dequeue'' (e:inb) out

-- | function remove element from front of Dequeue'
popFrontDEQ'' (Dequeue'' inb out) = if isEmptyDEQ'' (Dequeue'' inb out)
     then Nothing
     else if null inb
          then popFrontDEQ'' (Dequeue'' (reverse out) [])
          else Just (head inb, (Dequeue'' (tail inb) out))

-- | function add element on end of Dequeue'
pushBackDEQ'' (Dequeue'' inb out) e = Dequeue'' inb (e:out)

-- | function remove element from end of Dequeue'
popBackDEQ'' (Dequeue'' inb out) = if isEmptyDEQ'' (Dequeue'' inb out)
     then Nothing
     else if null out
          then popBackDEQ'' (Dequeue'' [] (reverse inb))
          else Just (head out, (Dequeue'' inb (tail out)))
