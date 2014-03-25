{-# LANGUAGE NoImplicitPrelude #-}

module Vector where

import Prelude(Show)

import Base
import Functional

class LikeVector v where
  map :: (a -> b) -> v a -> v b
  zip :: (a -> b -> c) -> v a -> v b -> v c
  fold :: (a -> a -> a) -> v a -> a

class LikeNumber a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a

instance LikeNumber Int where
  (+) = sumInt
  (*) = mulInt

instance LikeNumber Float where
  (+) = sumFloat
  (*) = mulFloat

instance LikeNumber Double where
  (+) = sumDouble
  (*) = mulDouble

data LikeVector2D a = LikeVector2D
                  { vx2D :: a,
                    vy2D :: a
                  } deriving Show

instance LikeVector LikeVector2D where
  map = map2D
  zip = zip2D
  fold = fold2D

map2D :: (a -> b) -> LikeVector2D a -> LikeVector2D b
-- map2D = join . ((. vy2D) .) . (. vx2D) . (LikeVector2D `on`)
-- map2D f = (vy2D `at`) . (vx2D `at`) . (on LikeVector2D) . ((f .) . at)
map2D = (((at vy2D) . (at vx2D) . (on LikeVector2D)) .) . (((. at) . (.)))
-- map2D f v = (LikeVector2D `on` f) (vx v) (vy v)

zip2D :: (a -> b -> c) -> LikeVector2D a -> LikeVector2D b -> LikeVector2D c
-- zip2D f (LikeVector2D x y) (LikeVector2D x' y') = LikeVector2D (f x x') $ f y y'
-- zip2D f v v' = LikeVector2D (f (vx2D v) (vx2D v')) (f (vy2D v) (vy2D v'))
zip2D f = join . (join $ (. (LikeVector2D .) . (. vx2D) . f . vx2D) . (.) . (flip (.)) . (. vy2D) . (f . vy2D))


fold2D :: (a -> a -> a) -> LikeVector2D a -> a
-- fold2D f (LikeVector2D  y) = f x y
-- fold2D f v = ((. vy2D) . (f . vx2D)) v v
-- fold2D = join . (((. vy2D) .) . (. vx2D))
-- fold2D f v = f (vx2D v) (vy2D v)
fold2D f = (((vy2D `at`) . (vx2D `at`)) . (on f) . at)


data LikeVector3D a = LikeVector3D
                  { vx3D :: a,
                    vy3D :: a,
                    vz3D :: a
                  }
                deriving Show

instance LikeVector LikeVector3D where
  map = map3D
  zip = zip3D
  fold = foldr3D
 
map3D :: (a -> b) -> LikeVector3D a -> LikeVector3D b
map3D f (LikeVector3D x y z) = f z `at` (LikeVector3D `on` f) x y

zip3D :: (a -> b -> c) -> LikeVector3D a -> LikeVector3D b -> LikeVector3D c
zip3D f (LikeVector3D x y z) (LikeVector3D x' y' z') = LikeVector3D (f x x') (f y y') (f z z')

foldr3D :: (a -> a -> a) -> LikeVector3D a -> a
-- foldr3D f (LikeVector3D x y z) = f x $ f y z
foldr3D f v = ((f . vx3D) v) $ ((. vz3D) . f . vy3D) v v

foldl3D :: (a -> a -> a) -> LikeVector3D a -> a
foldl3D f (LikeVector3D x y z) = f x y `f` z
-- foldl3D f v = f (f (vx3D v) (vy3D v)) (vz3D z)

innerProduct :: (LikeVector v, LikeNumber a) => v a -> v a -> a
innerProduct = fold (+) $$ zip (*)
-- innerProduct = ((fold (+)) .) . (zip (*))
