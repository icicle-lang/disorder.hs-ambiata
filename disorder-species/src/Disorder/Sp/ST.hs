{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Sp.ST (
  (=:)
, (+=)
, (-=)
, (.=)
, ap1
, ap2
, ap2'
, arrayToList
, arrayToList'
, ref
, read
, val
, write
, writeST
) where

import           Control.Monad
import           Control.Monad.ST

import           Data.STRef
import           Data.Array.ST hiding (index)

import           Prelude hiding (elem, read)

-------------------------
-- Syntactic sugar for ST
-------------------------

(=:) :: STRef s a -> a -> ST s ()
(=:) = writeSTRef

(.=) :: STRef s a -> (a -> a) -> ST s ()
(.=) = modifySTRef'

(+=) :: Num a => STRef s a -> a -> ST s ()
(+=) s a1 = (.=) s (+ a1)

(-=) :: Num a => STRef s a -> a -> ST s ()
(-=) s a1 = (.=) s (\a -> a - a1)

ap1 :: (a -> b) -> STRef s a -> ST s b
ap1 f a =
  do a' <- readSTRef a
     return $ f a'

ap2 :: (a -> b -> c) -> STRef s a -> STRef s b -> ST s c
ap2 f a b =
  do a' <- readSTRef a
     b' <- readSTRef b
     return $ f a' b'

ap2' :: (a -> b -> c) -> STRef s a -> b -> ST s c
ap2' f a b =
  do a' <- readSTRef a
     return $ f a' b

val :: STRef s a -> ST s a
val = readSTRef

ref :: ST s a -> ST s (STRef s a)
ref s =
  do s1 <- s
     newSTRef s1

arrayToList :: (Ix i, Enum i, Num i) => STArray s i a -> STRef s i -> ST s [a]
arrayToList ar i =
  do
    index <- readSTRef i
    forM [0..index] (readArray ar)

arrayToList' :: (Ix i, Enum i, Num i) => STArray s i a -> i -> ST s [a]
arrayToList' ar index =
  do
    forM [0..index] (readArray ar)

write :: Ix i => ST s (STArray s i e) -> STRef s i -> STRef s e -> ST s ()
write ar i e =
  do array <- ar
     index <- readSTRef i
     elem  <- readSTRef e
     writeArray array index elem

writeST :: Ix i => STArray s i e -> STRef s i -> ST s e -> ST s ()
writeST ar i e =
  do index <- readSTRef i
     elem  <- e
     writeArray ar index elem

read :: Ix i => ST s (STArray s i e) -> STRef s i -> ST s e
read ar i =
  do array <- ar
     index <- readSTRef i
     readArray array index
