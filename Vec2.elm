module Vec2 where

{-| A library for two-dimensional vector manipulations. Vectors are stored as
tuples of their x and y components, so you can use `(,)` as a constructor.

# Types
@docs Vec2

# Constant Vectors
@docs zero, one, right, left, up, down

# Arithmetic
@docs sum, difference, subtract, product, quotient, dot

# Scaling
@docs normalize, scale, scaleInverse, withLength, invert

# Rotating
@docs rotateLeft, rotateRight, rotate, rotateAround

# Transforming
@docs flipX, flipY, reciprocate, map

# Accessing
@docs theta, length, lengthSquared, angle

# Converting
@docs fromInts

# Drawing
@docs arrow
-}

{-| A two-dimensional vector is represented by its x and y components. -}
type Vec2 = (Float, Float)

{-| The zero vector. -}
zero : Vec2
zero = (0,0)

{-| A vector of ones: `(1,1)`. -}
one : Vec2
one = (1,1)

{-| A unit vector pointing to the right. -}
right : Vec2
right = (1,0)

{-| A unit vector pointing to the left. -}
left : Vec2
left = (-1,0)

{-| A unit vector pointing up. -}
up : Vec2
up = (0,1)

{-| A unit vector pointing down. -}
down : Vec2
down = (0,-1)

{-| Compute the componentwise sum of two vectors.

      sum (1,2) (3,4) == (4,6)
-}
sum : Vec2 -> Vec2 -> Vec2
sum = binary (+)

{-| Compute the componentwise difference between two vectors.

      difference (5,7) (3,4) == (2,3)
-}
difference : Vec2 -> Vec2 -> Vec2
difference = binary (-)

{-| Subtract the components of a vector from those of another.

      subtract (3,4) (5,7) == (2,3)
      map (subtract one) [(3,4), (1,5)] == [(2,3), (0,4)]
      
Note that `subtract` is the same thing as `flip difference`
-}
subtract : Vec2 -> Vec2 -> Vec2
subtract = flip difference

{-| Compute the componentwise product of two vectors.

      product (3,2) (3,4) == (9,8)
-}
product : Vec2 -> Vec2 -> Vec2
product = binary (*)

{-| Compute the componentwise quotient of two vectors.

      quotient (6,8) (3,2) == (2,4)
-}
quotient : Vec2 -> Vec2 -> Vec2
quotient = binary (/)

binary f (x,y) (x',y') = (f x x', f y y')

{-| Compute the dot product of two vectors.

      dot (2,3) (4,5) == 23
-}
dot : Vec2 -> Vec2 -> Float
dot (x,y) (x',y') = x*x' + y*y'

{-| Normalize a vector, i.e., compute a vector in the same direction with length 1.

      normalize (3,4) == (0.6,0.8)
      normalize x == x `withLength` 1
-}
normalize : Vec2 -> Vec2
normalize v = scaleInverse (length v) v

{-| Scale the components of a vector by a scalar factor.

      scale 3 (1,2) == (3,6)
-}
scale : Float -> Vec2 -> Vec2
scale u (x,y) = (x*u, y*u)

{-| Scale the components of a vector by the reciprocal of a scalar factor.

      scaleInverse 3 (3,6) == (1,2)
-}
scaleInverse : Float -> Vec2 -> Vec2
scaleInverse u (x, y) = (x/u, y/u)

{-| Scale a vector so it has the given length. Usually used as a binary operator,
e.g. ``(2,3) `withLength` 5``.

      (3,4) `withLength` 10 == (6,8)
-}
withLength : Vec2 -> Float -> Vec2
withLength v l = normalize v |> scale l

{-| Computes the square of the length of a vector. This is faster than
computing the length and can still be used to compare lengths, e.g.,
`lengthSquared (3,4) < lengthSquared (2, 5)`.

      lengthSquared (1,4) == 17
-}
lengthSquared : Vec2 -> Float
lengthSquared u = dot u u

{-| Computes the square of the length of a vector.

      length (3,4) == 5
-}
length : Vec2 -> Float
length = sqrt << lengthSquared

{-| Rotate a vector 90 degrees counterclockwise (to the left).

      rotateLeft (3,4) == (-4,3)

Note that `rotateLeft` is the same thing as `invert << rotateRight`.
-}
rotateLeft : Vec2 -> Vec2
rotateLeft (x,y) = (-y,x)

{-| Rotate a vector 90 degrees clockwise (to the right).

      rotateRight (3,4) == (4,-3)

Note that `rotateRight` is the same thing as `invert << rotateLeft`.
-}
rotateRight : Vec2 -> Vec2
rotateRight (x,y) = (y,-x)

{-| Rotate a vector 180 degrees, i.e., invert its components.

      invert (5,2) == (-5,-2)
-}
invert : Vec2 -> Vec2
invert (x,y) = (-x,-y)

{-| Compute the reciprocals of the components of a vector.

      reciprocate (2,4) == (0.5, 0.25)
-}
reciprocate : Vec2 -> Vec2
reciprocate (x,y) = (1/x,1/y)

{-| Rotate a vector counterclockwise by the given angle.

      rotate (degrees 90) (3,4) == (-4,3)
      rotate (degrees 90) up == left
-}
rotate : Float -> Vec2 -> Vec2
rotate a (x,y) =
  let (c,s) = (cos a,sin a) in
  (c*x - s*y, s*x + c*y)

{-| Rotate a vector counterclockwise around the given center by the given angle.

      rotateAround (1,1) (degrees 90) (3,4) == (-2,3)
-}
rotateAround : Vec2 -> Float -> Vec2 -> Vec2
rotateAround c a = sum c << rotate a << subtract c

{-| Reflect a vector horizontally (across the y axis).

      flipX (1,7) == (-1,7)
-}
flipX : Vec2 -> Vec2
flipX (x,y) = (-x,y)

{-| Reflect a vector vertically (across the x axis).

      flipY (1,7) == (1,-7)
-}
flipY : Vec2 -> Vec2
flipY (x,y) = (x,-y)
  
{-| Map a function over the components of a vector.

      map (\x -> x^2) (4,5) == (16,25)
-}
map : (Float -> Float) -> Vec2 -> Vec2
map f (x,y) = (f x, f y)

{-| Compute the angle between a vector and the positive x axis, i.e., the angle
between the vector and `right`.

      theta (1,1) == degrees 45
-}
theta : Vec2 -> Float
theta (x,y) = atan2 y x

{-| Compute the smaller angle between two vectors.

      angle up left == degrees 90
-}
angle : Vec2 -> Vec2 -> Float
angle u v = dot u v / length u / length v |> acos

{-| Convert a vector of ints to a Vec2

      lift fromInts Mouse.position
-}
fromInts : (Int,Int) -> Vec2
fromInts (x,y) = (toFloat x, toFloat y)

{-| Create a path which draws an arrow between two points.

      collage 400 400 [
        arrow 10 (0,0) (100,40) |> traced (solid blue)]
-}
arrow : Float -> Vec2 -> Vec2 -> Path
arrow r p q =
  let d = difference p q |> normalize
      e = scale r <| rotate (degrees -45) d
      f = scale r <| rotate (degrees 45) d in
  path [p, q, sum q e, q, sum q f]
