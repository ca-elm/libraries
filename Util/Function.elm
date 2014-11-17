module Util.Function where

uncurry3
  : (a -> b -> c -> d) ->
    (a, b, c) -> d
uncurry3 z (a, b, c) = z a b c

uncurry4
  : (a -> b -> c -> d -> e) ->
    (a, b, c, d) -> e
uncurry4 z (a, b, c, d) = z a b c d

uncurry5
  : (a -> b -> c -> d -> e -> f) ->
    (a, b, c, d, e) -> f 
uncurry5 z (a, b, c, d, e) = z a b c d e

uncurry6
  : (a -> b -> c -> d -> e -> f -> g) ->
    (a, b, c, d, e, f) -> g
uncurry6 z (a, b, c, d, e, f) = z a b c d e f

uncurry7
  : (a -> b -> c -> d -> e -> f -> g -> h) ->
    (a, b, c, d, e, f, g) -> h
uncurry7 z (a, b, c, d, e, f, g) = z a b c d e f g

uncurry8
  : (a -> b -> c -> d -> e -> f -> g -> h -> i) ->
    (a, b, c, d, e, f, g, h) -> i
uncurry8 z (a, b, c, d, e, f, g, h) = z a b c d e f g h