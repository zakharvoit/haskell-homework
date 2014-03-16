{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Base
import Functional

class Vector v where
  map :: (a -> b) -> v a -> v b
  zip :: (a -> b -> c) -> v a -> v b -> v c
  fold :: (a -> a -> a) -> v a -> a

class Number a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a

instance Number Int where
  (+) = sumInt
  (*) = mulInt

instance Number Float where
  (+) = sumFloat
  (*) = mulFloat
  
data Vector2D a = Vector2D a a

instance Vector Vector2D where
  map = map2D
  zip = zip2D
  fold = fold2D

map2D :: (a -> b) -> Vector2D a -> Vector2D b
map2D f (Vector2D x y) = (Vector2D `on` f) x y

zip2D :: (a -> b -> c) -> Vector2D a -> Vector2D b -> Vector2D c
zip2D f (Vector2D x y) (Vector2D x' y') = f y y' `at` Vector2D $ f x x'

fold2D :: (a -> a -> a) -> Vector2D a -> a
fold2D f (Vector2D x y) = f x y

data Vector3D a = Vector3D a a a

instance Vector Vector3D where
  map = map3D
  zip = zip3D
  fold = foldr3D
 
map3D :: (a -> b) -> Vector3D a -> Vector3D b
map3D f (Vector3D x y z) = f z `at` (Vector3D `on` f) x y

zip3D :: (a -> b -> c) -> Vector3D a -> Vector3D b -> Vector3D c
zip3D f (Vector3D x y z) (Vector3D x' y' z') = Vector3D (f x x') (f y y') (f z z')

foldr3D :: (a -> a -> a) -> Vector3D a -> a
foldr3D f (Vector3D x y z) = f x $ f y z

foldl3D :: (a -> a -> a) -> Vector3D a -> a
foldl3D f (Vector3D x y z) = f x y `f` z

innerProduct :: (Vector v, Number a) => v a -> v a -> a
innerProduct = fold (+) $$ zip (*)
