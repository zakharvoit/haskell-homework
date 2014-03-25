{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, FlexibleInstances #-}

module List where

import Base
import Vector
import qualified DataInterfaces as DI

instance LikeVector [] where
  fold = foldList
  zip = zipList
  map = mapList

foldList :: (a -> a -> a) -> [a] -> a
foldList _ [] = error "Zero length list"
foldList _ [x] = x
foldList f (x:xs) = f x (foldList f xs)

zipList :: (a -> b -> c) -> [a] -> [b] -> [c]
zipList _ [] [] = []
zipList _ xs [] = error "Lists have different length"
zipList _ [] ys = error "Lists have different length"
zipList f (x:xs) (y:ys) = f x y : zipList f xs ys

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

instance (LikeNumber a, LikeVector v) => LikeNumber (v a) where
  (+) = zip (+)
  (*) = zip (*)

likeVectorList :: DI.LikeVector []
likeVectorList = DI.LikeVector map zip fold

likeNumberVector :: DI.LikeNumber a -> DI.LikeVector v -> DI.LikeNumber (v a)
likeNumberVector (DI.LikeNumber (+) (*)) (DI.LikeVector { DI.isvZip = zip }) =
  DI.LikeNumber (zip (+)) (zip (*))

