{-# LANGUAGE NoImplicitPrelude #-}

module Functional(($), at, (.), (.>), on, ($$)) where

infixr 0 $
($) :: (a -> b) -> (a -> b)
f $ x = f x

at :: a -> (a -> b) -> b
x `at` f = f x
  
infixr 9 .
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)

-- In default library: >>>
(.>) :: (c -> d) -> (a -> c) -> (a -> d)
g .> f = g . f

infixl 0 `on`
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)

($$) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
($$) g f x y = g $ f x y

