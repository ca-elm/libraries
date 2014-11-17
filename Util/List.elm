module Util.List where

{-| A set of utility functions for lists 

# Accessing

@docs get, (@) 

# Advanced Mapping

@docs mapnl, mapnr

-}

{-| Random array access. Gets the nth element of a list. -}
get : Int -> [a] -> a
get n = head << drop n

{-| An infix version of get. Ex. [0, 1, 2] @ 1 => 2 -}
(@) : [a] -> Int -> a
(@) = flip get

{-| Map next left takes a function of three arguments a list, and an initital value, and applies the function to each element of the list, and the one following it. (The first element is paired with the base case.) This is the same as mapnr except it moves from left to right.

e.g. `mapnl (+) 1 [1, 2, 3] == [(1 + 1), (1 + 2), (2 + 3)]`

so `mapnl (+) 1 [1, 2, 3] == [2, 3, 5]`
-}
mapnl : (a -> a -> b) -> a -> [a] -> [b]
mapnl f s l
  = indexedMap (\i e -> 
      if i == 0 then f s e else f (get (i - 1) l) e ) l

{-| Map next right takes a function of three arguments a list, and an initital value, and applies the function to each element of the list, and the one before it. (The last element is paired with the base case.) This is the same as mapnl except it moves from right to left.

e.g. `mapnr (+) 1 [1, 2, 3] == [(1 + 2), (2 + 3), (3 + 1)]`

so `mapnr (+) 1 [1, 2, 3] == [3, 5, 4]`
-}
mapnr : (a -> a -> b) -> a -> [a] -> [b]
mapnr f s l
  = indexedMap (\i e ->
      if i == (length l) - 1 then f e s else f e (get (i + 1) l)) l

