{-# LANGUAGE NoImplicitPrelude #-}

module Functional(flip,join, ($), at, (.), (.>), on, ($$)) where

infixr 0 $
($) :: (a -> b) -> (a -> b)
-- f $ x = f x
($) f = f

at :: a -> (a -> b) -> b
at x f = f x
  
infixr 9 .
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)

-- In default library: >>>
(.>) :: (a -> c) -> (c -> d) -> (a -> d)
g .> f = f . g

infixl 0 `on`
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- on f g x y = f (g x) (g y)
-- on f g = ((((. (f .)) . (.)) . ((flip (.))))) g g
on = join . ((. ((flip (.)))) . ((. (.)) . ((flip (.)) . (.))))

($$) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
-- ($$) g f x y = g $ f x y
-- ($$) g f x = g . (f x)
-- ($$) g f = (g .) . f
($$) = (.) . (.)

join :: (a -> a -> b) -> a -> b
join f x = f x x

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x
