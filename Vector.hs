{-# LANGUAGE RankNTypes, NoImplicitPrelude, OverloadedStrings #-}

module Main where

import Base

data LikeVector v = LikeVector
                    { isvMap :: forall a b. (a -> b) -> v a -> v b
                    , isvZip :: forall a b c. (a -> b -> c) -> v a -> v b -> v c
                    , isvFold :: forall a. (a -> a -> a) -> v a -> a
                    }

data LikeNumber a = LikeNumber
                { (+) :: a -> a -> a
                , (*) :: a -> a -> a
                }

likeNumberInt :: LikeNumber Int
likeNumberInt = LikeNumber sumInt mulInt

likeNumberFloat :: LikeNumber Float
likeNumberFloat = LikeNumber sumFloat mulFloat

data Vector2D a = Vector2D a a

map2D :: (a -> b) -> Vector2D a -> Vector2D b
map2D f (Vector2D x y) = Vector2D (f x) (f y)

zip2D :: (a -> b -> c) -> Vector2D a -> Vector2D b -> Vector2D c
zip2D f (Vector2D x y) (Vector2D x' y') = Vector2D (f x x') (f y y')

fold2D :: (a -> a -> a) -> Vector2D a -> a
fold2D f (Vector2D x y) = f x y

likeVector2D :: LikeVector Vector2D
likeVector2D = LikeVector map2D zip2D fold2D

data Vector3D a = Vector3D a a a
 
map3D :: (a -> b) -> Vector3D a -> Vector3D b
map3D f (Vector3D x y z) = Vector3D (f x) (f y) (f z)

zip3D :: (a -> b -> c) -> Vector3D a -> Vector3D b -> Vector3D c
zip3D f (Vector3D x y z) (Vector3D x' y' z') = Vector3D (f x x') (f y y') (f z z')

foldr3D :: (a -> a -> a) -> Vector3D a -> a
foldr3D f (Vector3D x y z) = f x (f y z)

foldl3D :: (a -> a -> a) -> Vector3D a -> a
foldl3D f (Vector3D x y z) = f (f x y) z
 
likeVector3D :: LikeVector Vector3D
likeVector3D = LikeVector map3D zip3D foldr3D

innerProduct :: LikeVector v -> LikeNumber a -> v a -> v a -> a
innerProduct (LikeVector _ zip fold) (LikeNumber (+) (*)) v1 v2 = fold (+) (zip (*) v1 v2)
