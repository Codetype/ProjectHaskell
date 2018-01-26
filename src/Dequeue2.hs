{-| Module      : Dequeue2
    Description : This module contains Dequeue ADT implementation with definition of the constructor
                  and functions, which let make some operations on it.
    Copyright   : (c) Paweł Gędłek, 2018
                      Rafał Cegielski, 2018
    License     : BSD3
    Maintainer  : pawelgedlek@gmail.com
-}

module Dequeue2
  ( Dequeue'(MkDequeue')
  , emptyDEQ'       -- :: Dequeue a
  , isEmptyDEQ'     -- :: Dequeue a -> Bool
  , lengthDEQ'      -- :: Dequeue a -> Int
  , firstDEQ'       -- :: Dequeue a -> Maybe a
  , lastDEQ'        -- :: Dequeue a -> Maybe a
  , takeFrontDEQ'   -- :: Int -> Dequeue a -> [a]
  , takeBackDEQ'    -- :: Int -> Dequeue a -> [a]
  , pushFrontDEQ'   -- :: Dequeue a -> a -> Dequeue a
  , popFrontDEQ'    -- :: Dequeue a -> Maybe(a, Dequeue a)
  , pushBackDEQ'    -- :: Dequeue a -> a -> Dequeue a
  , popBackDEQ'     -- :: Dequeue a -> Maybe(a, Dequeue a)
  ) where

--interface (signature, contract)

emptyDEQ'     :: Dequeue' a
isEmptyDEQ'   :: Dequeue' a -> Bool
lengthDEQ'    :: Dequeue' a -> Int
firstDEQ'     :: Dequeue' a -> Maybe a
lastDEQ'      :: Dequeue' a -> Maybe a
takeFrontDEQ' :: Int -> Dequeue' a -> [a]
takeBackDEQ'  :: Int -> Dequeue' a -> [a]
pushFrontDEQ' :: Dequeue' a -> a -> Dequeue' a
popFrontDEQ'  :: Dequeue' a -> Maybe(a, Dequeue' a)
pushBackDEQ'  :: Dequeue' a -> a -> Dequeue' a
popBackDEQ'   :: Dequeue' a -> Maybe(a, Dequeue' a)

--implementation
-- | Dequeue' new type constructor
newtype Dequeue' a = MkDequeue' [a] deriving (Eq, Show)

-- | function create empty Dequeue' instance
emptyDEQ' = MkDequeue' []

-- | function check, is Dequeue' is empty
isEmptyDEQ' (MkDequeue' dq) = null dq

-- | function return length of Dequeue'
lengthDEQ' (MkDequeue' dq) = length dq

-- | function return first element of Dequeue'
firstDEQ' (MkDequeue' dq) = if isEmptyDEQ' (MkDequeue' dq)
   then Nothing
   else Just (head dq)

-- | function return last element of Dequeue'
lastDEQ' (MkDequeue' dq) = if isEmptyDEQ' (MkDequeue' dq)
    then Nothing
    else Just (last dq)

-- | function return first n elements from Dequeue'
takeFrontDEQ' x (MkDequeue' dq) =  take x dq

-- | function return last n elements from Dequeue'
takeBackDEQ' x (MkDequeue' dq) = drop (length dq - x) dq

-- | function add element on front of Dequeue'
pushFrontDEQ' (MkDequeue' dq) x = MkDequeue' (x:dq)

-- | function remove element from front of Dequeue'
popFrontDEQ' (MkDequeue' dq) = if isEmptyDEQ' (MkDequeue' dq)
                           then Nothing
                           else Just (head dq, MkDequeue'(tail dq))

-- | function add element on end of Dequeue'
pushBackDEQ' (MkDequeue' dq) x = MkDequeue' (dq++[x])

-- | function remove element from end of Dequeue'
popBackDEQ' (MkDequeue' dq) = if isEmptyDEQ' (MkDequeue' dq)
                           then Nothing
                           else Just (last dq, MkDequeue' (takeFrontDEQ' ((length dq) - 1) (MkDequeue' dq) ))
