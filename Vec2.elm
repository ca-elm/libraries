module Vec2 where

{-| A library for two-dimensional vector manipulations. Vectors are stored as
tuples of their x and y components, so you can use `(,)` as a constructor.

# Types
@docs Vec2

# Constant Vectors
@docs zero2, one2, right2, left2, up2, down2

# Operators
@docs @+, @-, @*, @/, @.

# Scaling
@docs unit2, scale2, scalei2, withlen2, neg2, inv2

# Transforming
@docs rotl2, rotr2, rot2, fliph2, flipv2, map2

# Accessing
@docs theta2, len2, lensqr2, angle2

# Drawing
@docs arrow
-}

{-| A two-dimensional vector is represented by its x and y components.
-}
type Vec2 = (Float, Float)

{-| The zero vector. -}
zero2 : Vec2
zero2 = (0,0)
{-| A vector of ones: `(1,1)`. -}
one2 : Vec2
one2 = (1,1)
{-| A unit vector pointing to the right. -}
right2 : Vec2
right2 = (1,0)
{-| A unit vector pointing to the left. -}
left2 : Vec2
left2 = (-1,0)
{-| A unit vector pointing up. -}
up2 : Vec2
up2 = (0,1)
{-| A unit vector pointing down. -}
down2 : Vec2
down2 = (0,-1)

{-| Add the components of two vectors.

      (1,2) @+ (3,4) == (4,6)
-}
(@+) : Vec2 -> Vec2 -> Vec2
(@+) = bin2 (+)

{-| Subtract the components of two vectors.

      (5,7) @- (3,4) == (2,3)
-}
(@-) : Vec2 -> Vec2 -> Vec2
(@-) = bin2 (-)

{-| Multiply the components of two vectors.

      (3,2) @* (3,4) == (9,8)
-}
(@*) : Vec2 -> Vec2 -> Vec2
(@*) = bin2 (*)

{-| Divide the components of two vectors.

      (6,8) @/ (3,2) == (2,4)
-}
(@/) : Vec2 -> Vec2 -> Vec2
(@/) = bin2 (/)

{-| Compute the dot product of two vectors.

      (2,3) @. (4,5) == 23
-}
(@.) : Vec2 -> Vec2 -> Float
(x,y) @. (x',y') = x*x' + y*y'

bin2 f (x,y) (x',y') = (f x x', f y y')

{-| Normalize a vector, i.e., compute a vector in the same direction with length 1.

      unit2 (3,4) == (0.6,0.8)
      unit2 x == x `withlen2` 1
-}
unit2 : Vec2 -> Vec2
unit2 v = scalei2 (len2 v) v

{-| Scale the components of a vector by a scalar factor.

      scale2 3 (1,2) == (3,6)
-}
scale2 : Float -> Vec2 -> Vec2
scale2 u (x,y) = (x*u, y*u)

{-| Scale the components of a vector by the reciprocal of a scalar factor.

      scalei2 3 (3,6) == (1,2)
      scale2 x . scalei2 x == id
-}
scalei2 : Float -> Vec2 -> Vec2
scalei2 u (x, y) = (x/u, y/u)

{-| Scale a vector so it has the given length. Usually used as a binary operator,
e.g. ``(3,4) `withlen2` 10``.

      (3,4) `withlen2` 10 == (6,8)
-}
withlen2 : Vec2 -> Float -> Vec2
withlen2 v l = unit2 v |> scale2 l

{-| Computes the square of the length of a vector. This is faster and can be
used to compare lengths, e.g., `lensqr2 (3,4) < lensqr2 (2, 5)`.

      lensqr2 (1,4) == 17
-}
lensqr2 : Vec2 -> Float
lensqr2 u = u @. u

{-| Computes the square of the length of a vector.

      len2 (3,4) == 5
-}
len2 : Vec2 -> Float
len2 = sqrt << lensqr2

{-| Rotate a vector 90 degrees counterclockwise (to the left).

      rotl2 (3,4) == (-4,3)
-}
rotl2 : Vec2 -> Vec2
rotl2 (x,y) = (-y,x)

{-| Rotate a vector 90 degrees clockwise (to the right).

      rotr2 (3,4) == (4,-3)
      rotr2 == neg2 . rotl2
-}
rotr2 : Vec2 -> Vec2
rotr2 (x,y) = (y,-x)

{-| Rotate a vector 180 degrees, i.e., invert its components.

      neg2 (5,2) == (-5,-2)
-}
neg2 : Vec2 -> Vec2
neg2 (x,y) = (-x,-y)

{-| Compute the reciprocals of the components of a vector.

      inv2 (2,4) == (0.5, 0.25)
-}
inv2 : Vec2 -> Vec2
inv2 (x,y) = (1/x,1/y)

{-| Rotate a vector counterclockwise by the given angle.

      rot2 (degrees 90) (3,4) == (-4,3)
      rot2 (degrees 90) up2 == left2
-}
rot2 : Float -> Vec2 -> Vec2
rot2 a (x,y) =
  let (c,s) = (cos a,sin a) in
  (c*x - s*y, s*x + c*y)

{-| Reflect a vector horizontally (across the y axis).

      fliph2 (1,7) == (-1,7)
-}
fliph2 : Vec2 -> Vec2
fliph2 (x,y) = (-x,y)

{-| Reflect a vector vertically (across the x axis).

      flipv2 (1,7) == (1,-7)
-}
flipv2 : Vec2 -> Vec2
flipv2 (x,y) = (x,-y)
  
{-| Map a function over the components of a vector.

      map2 (\x -> x^2) (4,5) == (16,25)
-}
map2 : (Float -> Float) -> Vec2 -> Vec2
map2 f (x,y) = (f x, f y)

{-| Compute the angle between a vector and the positive x axis, i.e., the angle
between the vector and `right2`.

      theta2 (1,1) == degrees 45
-}
theta2 : Vec2 -> Float
theta2 (x,y) = atan2 y x

{-| Compute the smaller angle between two vectors.

      angle2 up2 left2 == degrees 90
-}
angle2 : Vec2 -> Vec2 -> Float
angle2 u v = (u @. v) / len2 u / len2 v |> acos

{-| Create a path which draws an arrow between two points.

      main = collage 400 400 [
        arrow 10 (0,0) (100,40) |> traced (solid blue)]
-}
arrow : Float -> Vec2 -> Vec2 -> Path
arrow r p q =
  let d = p @- q |> unit2
      e = r `scale2` rot2 (degrees -45) d
      f = r `scale2` rot2 (degrees 45) d in
  path [p, q, q @+ e, q, q @+ f]
