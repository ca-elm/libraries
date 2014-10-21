module Vec2.Infix where

import Vec2 as V

type Vec2 = V.Vec2

(!+) = V.sum
(!-) = V.difference
(!*) = V.product
(!/) = V.quotient
(!.) = V.dot
