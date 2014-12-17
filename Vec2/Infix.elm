module Vec2.Infix where

{-| An extension to the vector library which adds infix operators. You should
import this module with an open import, i.e., `import Vec2.Infix (..)`.

# Types
@docs Vec2

# Operators
@docs (!+), (!-), (!*), (!/), (!.)
-}

import Vec2

{-| For convenience, this module also exports the `Vec2` type. -}
type alias Vec2 = Vec2.Vec2

(!+) : Vec2 -> Vec2 -> Vec2
(!+) = Vec2.sum

(!-) : Vec2 -> Vec2 -> Vec2
(!-) = Vec2.difference

(!*) : Vec2 -> Vec2 -> Vec2
(!*) = Vec2.product

(!/) : Vec2 -> Vec2 -> Vec2
(!/) = Vec2.quotient

(!.) : Vec2 -> Vec2 -> Float
(!.) = Vec2.dot
